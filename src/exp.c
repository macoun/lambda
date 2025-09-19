//
//  exp.c
//  Lisper
//
//  Created by Ferhat Ayaz on 13/03/2016.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#include "exp.h"
#include "mem.h"
#include "logger.h"
#include "printer.h"

#include <stdarg.h>
#include <string.h>
#include <assert.h>

bool is_eq(expr a, expr b)
{
  if (a.type == b.type)
  {
    if (is_sym(a))
      return (!strcmp(a.str, b.str));
    if (is_scoped_sym(a))
      return (!strcmp(a.array[0].str, b.array[0].str) &&
              a.array[1].value == b.array[1].value);
    return a.value == b.value;
  }
  return false;
}

bool is_equal(expr a, expr b)
{
  if (a.type == b.type)
  {
    if (is_num(a))
    {
      return (a.ulongv == b.ulongv);
    }
    else if (is_sym(a) || is_str(a))
    {
      return !strcmp(a.str, b.str);
    }
    return a.value == b.value;
  }

  return 0;
}

expr cons(struct machine *m, expr a, expr b)
{
  expr pair;

  pair.cell_type = STRUCT;
  pair.type = PAIR;

  machine_push(m, a);
  machine_push(m, b);
  pair.array = memory_alloc(m->memory, 2, true);
  machine_pop(m, &b);
  machine_pop(m, &a);

  pair.array[0] = a;
  pair.array[1] = b;
  return pair;
}

expr list(struct machine *m, expr first, ...)
{
  va_list ap;
  expr head, tail, cur;

  head = cons(m, first, NIL);
  tail = head;

  va_start(ap, first);
  for (cur = va_arg(ap, expr); !is_nil(cur);
       cur = va_arg(ap, expr))
  {
    set_cdr(tail, cons(m, cur, NIL));
    tail = cdr(tail);
  }
  va_end(ap);

  return head;
}

expr listn(struct machine *m, int n, ...)
{
  int i;
  expr l = NIL, cur;

  va_list ap;

  va_start(ap, n);
  for (i = 0; i < n; i++)
  {
    cur = va_arg(ap, expr);
    machine_push(m, cur);
  }
  va_end(ap);

  for (i = 0; i < n; i++)
  {
    machine_pop(m, &cur);
    l = cons(m, cur, l);
  }

  return l;
}

expr memq(expr memb, expr lst)
{
  while (is_pair(lst))
  {
    if (is_eq(memb, car(lst)))
      return lst;
    lst = cdr(lst);
  }
  return FALSE;
}

expr assoq(expr key, expr alist)
{
  while (is_pair(alist))
  {
    expr pair = car(alist);
    if (is_pair(pair) && is_eq(key, car(pair)))
      return pair;
    alist = cdr(alist);
  }
  return FALSE;
}

expr list_tail(expr l)
{
  while (is_pair(l) && is_pair(cdr(l)))
    l = cdr(l);
  return l;
}

int is_list(expr e)
{
  return is_nil(cdr(list_tail(e)));
}

long list_length(expr l)
{
  long len;

  len = 0;
  while (is_pair(l))
  {
    l = cdr(l);
    len++;
  }
  return len;
}

expr list_append_x(struct machine *m, expr l, expr e)
{
  expr tail;

  if (is_nil(l))
    return e;

  tail = list_tail(l);
  set_cdr(tail, e);
  return l;
}

// !! recursive version
expr list_append(struct machine *m, expr a, expr b)
{
  if (is_nil(a))
    return b;
  return cons(m, car(a), list_append(m, cdr(a), b));
}

expr list_append2(struct machine *m, expr l, expr e)
{
  expr head;

  if (is_nil(l))
    return e;

  head = NIL;
  while (!is_nil(l))
  {
    head = cons(m, car(l), head);
    l = cdr(l);
  }
  while (!is_nil(e))
  {
    head = cons(m, car(e), head);
    e = cdr(e);
  }
  expr reversed = list_reverse(m, head);
  return reversed;
}

expr list_ref(expr l, expr n)
{
  long idx;

  if (!is_num(n) || n.longv < 0)
  {
    error("Invalid index");
    return NIL;
  }

  idx = n.longv;
  while (is_pair(l) && idx > 0)
  {
    l = cdr(l);
    idx--;
  }

  if (idx == 0 && is_pair(l))
    return car(l);

  error("Index out of bounds");
  return NIL;
}

// Reverse a list, preserving the original list
expr list_reverse(struct machine *m, expr lst)
{
  expr result = NIL;
  expr current = lst;

  while (!is_nil(current))
  {
    result = cons(m, car(current), result);
    current = cdr(current);
  }

  return result;
}

expr vector(struct machine *m, long length, expr *exps)
{
  expr vect;
  int i;

  vect.cell_type = ARRAY;
  vect.type = VECTOR;
  vect.array = memory_alloc(m->memory, length + 1, true);
  vect.array[0] = mk_num(length);

  for (i = 0; i < length; i++)
    vect.array[i + 1] = exps ? exps[i] : NIL;

  return vect;
}
