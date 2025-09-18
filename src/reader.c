//
//  reader.c
//  Lisper
//
//  Created by Ferhat Ayaz on 10/03/16.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#include "reader.h"
#include "logger.h"
#include "machine.h"
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdarg.h>

static expr parse_pair(struct machine *m, const char **sp, char *brk, int *error);
static expr parse_quote(struct machine *m, const char **sp, int *error);
static expr parse_string(const char **sp, int *error);
static expr parse_number(const char **sp, int *error);
static expr parse_symbol(const char **sp, int *error);
static expr parse_quasiquote(struct machine *m, const char **sp, int *error);
static expr parse_unquote(struct machine *m, const char **sp, int *error);
static char *read_file(const char *fname);

static expr parse_pair(struct machine *m, const char **sp, char *brk, int *error)
{
  const char *s;
  expr first, last = NIL, res, tmp;
  int lastexp;

  s = *sp;
  assert(*s == brk[0]);
  s++;

  first = NIL;
  lastexp = 0;

  // Skip whitespaces
  s += strspn(s, "\t\n ");

  while (*s != brk[1])
  {
    machine_push(m, last);
    res = parse_exp(m, &s, error);
    machine_pop(m, &last);

    if (*error)
      return NIL;

    s += strspn(s, "\t\n "); // skip whitespace

    if (*s == '\0')
    {
      *error = PERR_TERM;
      return NIL;
    }

    if (is_nil(first))
    {
      first = last = cons(m, res, NIL);
      machine_push(m, first);
    }
    else
    {
      if (lastexp)
      {
        set_cdr(last, res);
      }
      else
      {
        machine_push(m, last);
        tmp = cons(m, res, NIL);
        machine_pop(m, &last);
        set_cdr(last, tmp);
        last = cdr(last);
      }
    }
    if (*s == '.' && strchr("\t\n ", *(s + 1)))
    {
      lastexp = 1;
      s++;
    }
  }
  *sp = s + 1;
  if (!is_nil(first))
    machine_pop(m, &first);
  return first;
}

static expr parse_quote(struct machine *m, const char **sp, int *error)
{
  const char *s;
  expr exp;

  s = *sp;
  assert(*s == '\'');
  s++;

  if (strchr("\t\r\n \0", *s))
  {
    *error = PERR_INV_QUOTE;
    return NIL;
  }

  exp = parse_exp(m, &s, error);
  exp = cons(m, exp, NIL);
  exp = cons(m, mk_sym("quote"), exp);

  *sp = s;
  return exp;
}

static expr parse_string(const char **sp, int *error)
{
  const char *s;
  expr exp;
  char esc;

  s = *sp;
  assert(*s == '"');
  s++;

  esc = 0;

  while (*s != '"' || esc)
  {
    if (*s == '\0')
    {
      *error = PERR_TERM;
      return NIL;
    }

    esc = (*s == '\\');
    s++;
  }

  exp = mk_str(strndup(*sp + 1, s - (*sp) - 1));
  *sp = s + 1;
  return exp;
}

static expr parse_number(const char **sp, int *error)
{
  const char *s;
  char *endptr;

  s = *sp;
  assert(isdigit(*s) || (*s == '-' && isdigit(*(s + 1))));

  // double val = strtod(s, &endptr);
  // if (endptr != s)
  // {
  //   *sp = endptr;
  //   return mk_float(val);
  // }

  long intv = strtol(s, &endptr, 10);
  if (endptr != s)
  {
    *sp = endptr;
    return mk_num(intv);
  }

  *error = PERR_INV_NUMBER;
  return NIL;
}

static expr parse_symbol(const char **sp, int *error)
{
  char *s;
  expr exp;

  s = strpbrk(*sp, "\t\r\n()[]\"'`, ");
  if (s != NULL)
  {
    exp = mk_sym(strndup(*sp, s - (*sp)));
    *sp = s;
  }
  else
  {
    exp = mk_sym(strdup(*sp));
    *sp = *sp + strlen(*sp);
  }
  return exp;
}

static expr parse_quasiquote(struct machine *m, const char **sp, int *error)
{
  const char *s;
  expr exp;

  s = *sp;
  assert(*s == '`');
  s++;

  if (strchr("\t\r\n \0", *s))
  {
    *error = PERR_INV_QUASIQUOTE;
    return NIL;
  }

  exp = parse_exp(m, &s, error);
  exp = cons(m, exp, NIL);
  exp = cons(m, mk_sym("quasiquote"), exp);

  *sp = s;
  return exp;
}

static expr parse_unquote(struct machine *m, const char **sp, int *error)
{
  const char *s;
  expr exp;
  char *symbol_name = "unquote";

  s = *sp;
  assert(*s == ',');
  s++;

  // Check for @ to handle unquote-splicing
  if (*s == '@')
  {
    symbol_name = "unquote-splicing";
    s++;
  }

  if (strchr("\t\r\n \0", *s))
  {
    *error = PERR_INV_UNQUOTE;
    return NIL;
  }

  exp = parse_exp(m, &s, error);
  exp = cons(m, exp, NIL);
  exp = cons(m, mk_sym(symbol_name), exp);

  *sp = s;
  return exp;
}
expr parse_exp(struct machine *m, const char **sp, int *error)
{
  expr res;
  const char *s;
  char ch;
  size_t invlen;

  *error = PERR_OK;
  s = *sp;

  for (;;)
  {
    // Skip whitespaces
    invlen = strspn(s, "\t\n ");
    s += invlen;
    // Skip comments
    if (*s == ';')
    {
      while (*s && *s != '\n')
        s++;
    }
    else
    {
      break;
    }
  }

  ch = *s;

  if (ch == '(' || ch == '[')
  {
    res = parse_pair(m, &s, ch == '(' ? "()" : "[]", error);
  }
  else if (ch == '\'')
  {
    res = parse_quote(m, &s, error);
  }
  else if (ch == '`')
  {
    res = parse_quasiquote(m, &s, error);
  }
  else if (ch == ',')
  {
    res = parse_unquote(m, &s, error);
  }
  else if (ch == '"')
  {
    res = parse_string(&s, error);
  }
  else if (isdigit(ch) || (ch == '-' && isdigit(*(s + 1))))
  {
    // TODO: we have to check for '-' followed by a digit to distinguish from a symbol
    // like '-foo which is valid, However -2m is also a valid symbol.
    // We need to assert that all following chars are digits or dots
    // otherwise it is a symbol.
    res = parse_number(&s, error);
  }
  else if (ch != '\0' && !strspn(s, ";)]"))
  {
    res = parse_symbol(&s, error);
    if (!strcmp(res.str, "#t"))
    {
      free(res.str);
      res = TRUE;
    }
    else if (!strcmp(res.str, "#f"))
    {
      free(res.str);
      res = FALSE;
    }
  }
  else if (ch != '\0')
  {
    error("Unknown char: '%c'\n", ch);
    *error = PERR_UNK_CHAR;
    res = NIL;
  }
  else
  {
    *error = PERR_FIN;
    return NIL;
  }

  *sp = s;
  return res;
}

#pragma mark -

expr parse_from_string(struct machine *m, const char *buffer, int *err)
{
  expr exp, source = NIL;
  memory_enable_gc(m->memory, false);

  for (const char *p = buffer; *p != '\0';)
  {
    exp = parse_exp(m, &p, err);

    if (*err == PERR_FIN)
      break;

    if (*err != 0)
      return FALSE;

    source = cons(m, exp, source);
  }

  source = list_reverse(m, source);
  memory_enable_gc(m->memory, true);
  return source;
}

expr parse_from_file(struct machine *m, const char *fname, int *err)
{
  char *buffer;
  expr source;

  buffer = read_file(fname);
  source = parse_from_string(m, buffer, err);
  free(buffer);

  return source;
}

static char *read_file(const char *fname)
{
  char *buffer;
  long size;
  FILE *f;

  f = fopen(fname, "r");
  if (f == NULL)
  {
    fprintf(stderr, "Could not open %s\n", fname);
    return NULL;
  }

  fseek(f, 0L, SEEK_END);
  size = ftell(f);
  rewind(f);

  buffer = calloc(size + 1, sizeof(char));
  fread(buffer, 1, size, f);
  *(buffer + size) = '\0';

  return buffer;
}