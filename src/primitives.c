//
//  primitives.c
//  Lisper
//
//  Created by Ferhat Ayaz on 19/03/16.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#include "primitives.h"
#include "printer.h"
#include "logger.h"
#include "machine.h"

expr op_add(struct machine *m, expr args)
{
  expr cur;
  unsigned long sum = 0;

  while (!is_nil(args))
  {
    cur = car(args);
    if (is_num(cur))
    {
      sum += cur.intv;
    }
    else
    {
      error("%s: Invalid operand %d", __PRETTY_FUNCTION__, cur.type);
      break;
    }
    args = cdr(args);
  }
  return mk_num(sum);
}

expr op_sub(struct machine *m, expr args)
{
  expr cur;
  long sub;

  if (is_nil(cdr(args)))
  {
    error("Too few arguments");
    return mk_num(0);
  }

  cur = car(args);
  args = cdr(args);
  sub = cur.intv;
  while (!is_nil(args))
  {
    cur = car(args);
    if (is_num(cur))
    {
      sub -= cur.intv;
    }
    else
    {
      error("Invalid operand %d", cur.type);
      break;
    }
    args = cdr(args);
  }
  return mk_num(sub);
}

expr op_mul(struct machine *m, expr args)
{
  expr cur;
  unsigned long prod = 1;

  while (!is_nil(args))
  {
    cur = car(args);
    if (is_num(cur))
    {
      prod *= cur.intv;
    }
    else
    {
      error("Invalid operand %d", cur.type);
      break;
    }
    args = cdr(args);
  }
  return mk_num(prod);
}

#pragma mark -

expr op_eq(struct machine *m, expr args)
{
  expr ref, cur;

  if (is_nil(cdr(args)))
  {
    error("Too few arguments");
    return mk_num(0);
  }

  ref = car(args);
  args = cdr(args);

  while (is_pair(args))
  {
    cur = car(args);
    args = cdr(args);
    if (!is_equal(ref, cur))
      return mk_num(0);
  }
  return mk_num(1);
}

expr op_gt(struct machine *m, expr args)
{
  expr ref, cur;

  if (is_nil(cdr(args)))
  {
    error("Too few arguments");
    return mk_num(0);
  }

  ref = car(args);
  args = cdr(args);

  while (is_pair(args))
  {
    cur = car(args);
    args = cdr(args);
    if (ref.longv <= cur.longv)
      return mk_num(0);
  }
  return mk_num(1);
}

expr op_lt(struct machine *m, expr args)
{
  expr ref, cur;

  if (is_nil(cdr(args)))
  {
    error("Too few arguments");
    return mk_num(0);
  }

  ref = car(args);
  args = cdr(args);

  while (is_pair(args))
  {
    cur = car(args);
    args = cdr(args);
    if (ref.longv >= cur.longv)
      return mk_num(0);
  }
  return mk_num(1);
}

#pragma mark -

expr op_car(struct machine *m, expr args)
{
  return caar(args);
}

expr op_cdr(struct machine *m, expr args)
{
  return cdar(args);
}

expr op_cons(struct machine *m, expr args)
{
  return cons(m, car(args), cadr(args));
}

expr op_list(struct machine *m, expr args)
{
  return args;
}

#pragma mark -

expr op_print(struct machine *m, expr args)
{
  expr current;
  for (; !is_nil(args); args = cdr(args))
  {
    current = car(args);
    if (current.type == STRING)
      printf("%s", current.str);
    else
      print_exp(car(args));
    if (!is_nil(cdr(args)))
      printf(" ");
  }
  return mk_num(12);
}

expr op_println(struct machine *m, expr args)
{
  op_print(m, args);
  printf("\n");
  return mk_num(13);
}

#pragma mark -

expr op_is_null(struct machine *m, expr args)
{
  long b;
  b = is_nil(car(args)) ? 1 : 0;
  return mk_num(b);
}

expr op_is_number(struct machine *m, expr args)
{
  long b;
  b = is_num(car(args)) ? 1 : 0;
  return mk_num(b);
}

expr op_is_string(struct machine *m, expr args)
{
  long b;
  b = is_str(car(args)) ? 1 : 0;
  return mk_num(b);
}

expr op_is_pair(struct machine *m, expr args)
{
  long b;
  b = is_pair(car(args)) ? 1 : 0;
  return mk_num(b);
}

expr op_is_symbol(struct machine *m, expr args)
{
  long b;
  b = is_sym(car(args)) ? 1 : 0;
  return mk_num(b);
}

#pragma mark -

expr op_is_zero(struct machine *m, expr args)
{
  long b;
  expr arg;

  arg = car(args);
  b = (is_num(arg) && arg.intv == 0) ? 1 : 0;
  return mk_num(b);
}

expr op_is_positive(struct machine *m, expr args)
{
  long b;
  expr arg;

  arg = car(args);
  b = (is_num(arg) && arg.intv > 0) ? 1 : 0;
  return mk_num(b);
}

expr op_is_negative(struct machine *m, expr args)
{
  long b;
  expr arg;

  arg = car(args);
  b = (is_num(arg) && arg.intv < 0) ? 1 : 0;
  return mk_num(b);
}

expr op_is_odd(struct machine *m, expr args)
{
  long b;
  expr arg;

  arg = car(args);
  b = (is_num(arg) && abs(arg.intv) % 2 == 1) ? 1 : 0;
  return mk_num(b);
}

expr op_is_even(struct machine *m, expr args)
{
  long b;
  expr arg;

  arg = car(args);
  b = (is_num(arg) && arg.intv % 2 == 0) ? 1 : 0;
  return mk_num(b);
}

#pragma mark -

expr op_vector_create(struct machine *m, expr args)
{
  long size, i;
  expr exp, vect;

  size = list_length(args);

  machine_push(m, args);
  vect = vector(m, size, NULL);
  machine_pop(m, &args);

  exp = args;
  for (i = 0; i < size; i++)
  {
    vect_set(vect, i, car(exp));
    exp = cdr(exp);
  }

  return vect;
}

expr op_vector_size(struct machine *m, expr args)
{
  expr vect;

  vect = car(args);
  if (!is_vector(vect))
  {
    error("Vector expected");
    return mk_num(0);
  }
  return mk_num(vect_size(vect));
}

expr op_vector_get(struct machine *m, expr args)
{
  expr vect, idx;

  vect = car(args);
  if (!is_vector(vect))
  {
    error("Vector expected");
    return NIL;
  }

  if (is_nil(cdr(args)))
  {
    error("Missing index");
    return NIL;
  }

  idx = cadr(args);
  if (!is_num(idx))
  {
    error("Invalid index");
    return NIL;
  }

  return vect_get(vect, idx.longv);
}

expr op_vector_set(struct machine *m, expr args)
{
  expr vect, idx, item;

  vect = car(args);
  if (!is_vector(vect))
  {
    error("Vector expected");
    return NIL;
  }

  if (is_nil(cdr(args)))
  {
    error("Missing index");
    return NIL;
  }

  idx = cadr(args);
  if (!is_num(idx))
  {
    error("Invalid index");
    return NIL;
  }

  if (is_nil(cddr(args)))
  {
    error("Missing item");
    return NIL;
  }

  item = caddr(args);
  vect_set(vect, idx.longv, item);

  return mk_num(1);
}
