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
#include "env.h"
#include "match.h"
#include "expand.h"
#include <string.h>

expr op_add(struct machine *m, expr args)
{
  expr cur, ops;
  unsigned long sum = 0;
  ops = args;
  int i = 0;
  while (!is_nil(args))
  {
    cur = car(args);
    if (is_num(cur))
    {
      sum += cur.intv;
    }
    else
    {
      error("%s: Invalid operand %d (i=%d)", __PRETTY_FUNCTION__, cur.type, i);
      print_exp(ops);
      break;
    }
    args = cdr(args);
    i++;
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
      return FALSE;
  }
  return TRUE;
}

expr op_gt(struct machine *m, expr args)
{
  expr ref, cur;

  if (is_nil(cdr(args)))
  {
    error("Too few arguments");
    return FALSE;
  }

  ref = car(args);
  args = cdr(args);

  while (is_pair(args))
  {
    cur = car(args);
    args = cdr(args);
    if (ref.longv <= cur.longv)
      return FALSE;
  }
  return TRUE;
}

expr op_lt(struct machine *m, expr args)
{
  expr ref, cur;

  if (is_nil(cdr(args)))
  {
    error("Too few arguments");
    return FALSE;
  }

  ref = car(args);
  args = cdr(args);

  while (is_pair(args))
  {
    cur = car(args);
    args = cdr(args);
    if (ref.longv >= cur.longv)
      return FALSE;
  }
  return TRUE;
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

expr op_list_ref(struct machine *m, expr args)
{
  expr lst, idx;
  if (!is_pair(cdr(args)))
  {
    error("Too few arguments");
    return NIL;
  }
  lst = car(args);
  idx = cadr(args);
  return list_ref(lst, idx);
}

expr op_append(struct machine *m, expr args)
{
  expr a, b;
  if (!is_pair(cdr(args)))
  {
    error("Too few arguments");
    return NIL;
  }
  a = car(args);
  b = cadr(args);
  return list_append(m, a, b);
}

expr op_memq(struct machine *m, expr args)
{
  return memq(car(args), cadr(args));
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
  fflush(stdout);
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
  return is_nil(car(args)) ? TRUE : FALSE;
}

expr op_is_number(struct machine *m, expr args)
{
  return is_num(car(args)) ? TRUE : FALSE;
}

expr op_is_string(struct machine *m, expr args)
{
  return is_str(car(args)) ? TRUE : FALSE;
}

expr op_is_pair(struct machine *m, expr args)
{
  return is_pair(car(args)) ? TRUE : FALSE;
}

expr op_is_symbol(struct machine *m, expr args)
{
  return is_sym(car(args)) ? TRUE : FALSE;
}

#pragma mark -

expr op_is_zero(struct machine *m, expr args)
{
  expr arg;

  arg = car(args);
  return (is_num(arg) && arg.intv == 0) ? TRUE : FALSE;
}

expr op_is_positive(struct machine *m, expr args)
{
  expr arg;

  arg = car(args);
  return (is_num(arg) && arg.intv > 0) ? TRUE : FALSE;
}

expr op_is_negative(struct machine *m, expr args)
{
  expr arg;

  arg = car(args);
  return (is_num(arg) && arg.intv < 0) ? TRUE : FALSE;
}

expr op_is_odd(struct machine *m, expr args)
{
  expr arg;

  arg = car(args);
  return (is_num(arg) && abs(arg.intv) % 2 == 1) ? TRUE : FALSE;
}

expr op_is_even(struct machine *m, expr args)
{
  expr arg;

  arg = car(args);
  return (is_num(arg) && arg.intv % 2 == 0) ? TRUE : FALSE;
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
expr op_match_pattern(struct machine *m, expr args)
{
  expr pattern, exp, literals;

  pattern = car(args);
  exp = cadr(args);
  literals = caddr(args);

  return match_pattern(m, pattern, exp, literals);
}

expr op_expand_template(struct machine *m, expr args)
{
  expr template, bindings, patternvars;

  template = car(args);
  bindings = cadr(args);
  patternvars = caddr(args);

  return expand_template(m, template, 0, bindings, patternvars, NIL);
}

expr op_pattern_vars(struct machine *m, expr args)
{
  expr pattern, literals;

  pattern = car(args);
  literals = cadr(args);

  return pattern_vars(m, pattern, 0, literals, NIL);
}

expr primitives_env(struct machine *m)
{
  expr vars, vals;

  vars = listn(m, 33,
               mk_sym("+"),
               mk_sym("*"),
               mk_sym("-"),
               mk_sym("eq?"),
               mk_sym(">"),
               mk_sym("<"),
               mk_sym("null?"),
               mk_sym("number?"),
               mk_sym("string?"),
               mk_sym("pair?"),
               mk_sym("symbol?"),
               mk_sym("zero?"),
               mk_sym("positive?"),
               mk_sym("negative?"),
               mk_sym("odd?"),
               mk_sym("even?"),
               mk_sym("make-vector"),
               mk_sym("vector-length"),
               mk_sym("vector-ref"),
               mk_sym("vector-set!"),
               mk_sym("car"),
               mk_sym("cdr"),
               mk_sym("cons"),
               mk_sym("list"),
               mk_sym("append"),
               mk_sym("display"),
               mk_sym("newline"),
               mk_sym("println"),
               mk_sym("match-pattern"),
               mk_sym("memq"),
               mk_sym("list-ref"),
               mk_sym("expand-template"),
               mk_sym("pattern-vars"),
               NIL);
  vals = listn(m, 33,
               mk_prim(op_add),
               mk_prim(op_mul),
               mk_prim(op_sub),
               mk_prim(op_eq),
               mk_prim(op_gt),
               mk_prim(op_lt),
               mk_prim(op_is_null),
               mk_prim(op_is_number),
               mk_prim(op_is_string),
               mk_prim(op_is_pair),
               mk_prim(op_is_symbol),
               mk_prim(op_is_zero),
               mk_prim(op_is_positive),
               mk_prim(op_is_negative),
               mk_prim(op_is_odd),
               mk_prim(op_is_even),
               mk_prim(op_vector_create),
               mk_prim(op_vector_size),
               mk_prim(op_vector_get),
               mk_prim(op_vector_set),
               mk_prim(op_car),
               mk_prim(op_cdr),
               mk_prim(op_cons),
               mk_prim(op_list),
               mk_prim(op_append),
               mk_prim(op_print),
               mk_prim(op_println),
               mk_prim(op_println),
               mk_prim(op_match_pattern),
               mk_prim(op_memq),
               mk_prim(op_list_ref),
               mk_prim(op_expand_template),
               mk_prim(op_pattern_vars),
               NIL);
  return env_extend(m, vars, vals, NIL);
}
