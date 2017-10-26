//
//  reader.c
//  Lisper
//
//  Created by Ferhat Ayaz on 10/03/16.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#include "reader.h"
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdarg.h>

static expr parse_pair(char **sp, int *error)
{
  char *s;
  expr first, last = NIL, res, tmp;
  int lastexp;

  s = *sp;
  assert(*s == '(');
  s++;

  first = NIL;
  lastexp = 0;

  // Skip whitespaces
  s += strspn(s, "\t\n ");

  while (*s != ')')
  {
    push(last);
    res = parse_exp(&s, error);
    pop(last);

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
      first = last = cons(res, NIL);
      push(first);
    }
    else
    {
      if (lastexp)
      {
        set_cdr(last, res);
      }
      else
      {
        push(last);
        tmp = cons(res, NIL);
        pop(last);
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
    pop(first);
  return first;
}

static expr parse_quote(char **sp, int *error)
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

  exp = parse_exp(&s, error);
  exp = cons(exp, NIL);
  exp = cons(mk_sym("quote"), exp);

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

  s = strpbrk(*sp, "\t\r\n()\"' ");
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

expr parse_exp(char **sp, int *error)
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

  if (ch == '(')
  {
    res = parse_pair(&s, error);
  }
  else if (ch == '\'')
  {
    res = parse_quote(&s, error);
  }
  else if (ch == '"')
  {
    res = parse_string(&s, error);
  }
  else if (isdigit(ch))
  {
    res = parse_number(&s, error);
  }
  else if (ch != '\0' && !strspn(s, ";)"))
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
