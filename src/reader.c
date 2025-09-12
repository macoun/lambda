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

static expr parse_pair(struct machine *m, char **sp, char *brk, int *error)
{
  char *s;
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

static expr parse_quote(struct machine *m, char **sp, int *error)
{
  char *s;
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

static expr parse_string(char **sp, int *error)
{
  char *s;
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

  exp = mk_cell(STRING,
                strndup(*sp + 1, s - (*sp) - 1));
  *sp = s + 1;
  return exp;
}

static expr parse_number(char **sp, int *error)
{
  char *s;
  expr exp;

  s = *sp;
  assert(isdigit(*s));

  exp = mk_num(strtol(s, sp, 10));

  return exp;
}

static expr parse_symbol(char **sp, int *error)
{
  char *s;
  expr exp;

  s = strpbrk(*sp, "\t\r\n()[]\"'`, ");
  if (s != NULL)
  {
    exp = mk_cell(SYMBOL, strndup(*sp, s - (*sp)));
    *sp = s;
  }
  else
  {
    exp = mk_cell(SYMBOL, strdup(*sp));
    *sp = *sp + strlen(*sp);
  }
  return exp;
}
static expr parse_quasiquote(struct machine *m, char **sp, int *error)
{
  char *s;
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

static expr parse_unquote(struct machine *m, char **sp, int *error)
{
  char *s;
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
expr parse_exp(struct machine *m, char **sp, int *error)
{
  expr res;
  char *s;
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
  else if (isdigit(ch))
  {
    res = parse_number(&s, error);
  }
  else if (ch != '\0' && !strspn(s, ";)]"))
  {
    res = parse_symbol(&s, error);
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
