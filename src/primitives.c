//
//  primitives.c
//  Lisper
//
//  Created by Ferhat Ayaz on 19/03/16.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#include "primitives.h"
#include "printer.h"

expr op_add(expr args)
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

expr op_sub(expr args)
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

expr op_mul(expr args)
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

expr op_eq(expr args)
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

expr op_gt(expr args)
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

expr op_lt(expr args)
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

expr op_car(expr args)
{
  return caar(args);
}

expr op_cdr(expr args)
{
  return cdar(args);
}

expr op_cons(expr args)
{
  return cons(car(args), cadr(args));
}

expr op_list(expr args)
{
  return args;
}

#pragma mark -

expr op_print(expr args)
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

expr op_println(expr args)
{
  op_print(args);
  printf("\n");
  return mk_num(13);
}

#pragma mark -

expr op_is_null(expr args)
{
  long b;
  b = is_nil(car(args)) ? 1 : 0;
  return mk_num(b);
}

expr op_is_number(expr args)
{
  long b;
  b = is_num(car(args)) ? 1 : 0;
  return mk_num(b);
}

expr op_is_string(expr args)
{
  long b;
  b = is_str(car(args)) ? 1 : 0;
  return mk_num(b);
}

expr op_is_pair(expr args)
{
  long b;
  b = is_pair(car(args)) ? 1 : 0;
  return mk_num(b);
}

expr op_is_symbol(expr args)
{
  long b;
  b = is_sym(car(args)) ? 1 : 0;
  return mk_num(b);
}

#pragma mark -

expr op_is_zero(expr args)
{
  long b;
  expr arg;
  
  arg = car(args);
  b = (is_num(arg) && arg.intv == 0) ? 1 : 0;
  return mk_num(b);
}

expr op_is_positive(expr args)
{
  long b;
  expr arg;
  
  arg = car(args);
  b = (is_num(arg) && arg.intv > 0) ? 1 : 0;
  return mk_num(b);
}

expr op_is_negative(expr args)
{
  long b;
  expr arg;
  
  arg = car(args);
  b = (is_num(arg) && arg.intv < 0) ? 1 : 0;
  return mk_num(b);
}

expr op_is_odd(expr args)
{
  long b;
  expr arg;
  
  arg = car(args);
  b = (is_num(arg) && abs(arg.intv) % 2 == 1) ? 1 : 0;
  return mk_num(b);
}

expr op_is_even(expr args)
{
  long b;
  expr arg;
  
  arg = car(args);
  b = (is_num(arg) && arg.intv % 2 == 0) ? 1 : 0;
  return mk_num(b);
}


#pragma mark -

expr op_vector_create(expr args)
{
  long size, i;
  expr exp, vect;

  size = list_length(args);

  push(args);
  vect = vector(size, NULL);
  pop(args);

  exp = args;
  for (i = 0; i < size; i++)
  {
    vect_set(vect, i, car(exp));
    exp = cdr(exp);
  }

  return vect;
}

expr op_vector_size(expr args)
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

expr op_vector_get(expr args)
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

expr op_vector_set(expr args)
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
