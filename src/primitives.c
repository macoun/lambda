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
#include "pattern.h"
#include "evaluator.h"

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

expr op_mod(struct machine *m, expr args)
{
  expr x = car(args);
  expr y = cadr(args);

  if (!(is_num(x) && is_num(y)))
  {
    error("Not enough numeric arguments");
    return FALSE;
  }

  if (y.intv == 0)
  {
    error("Division by zero");
    return FALSE;
  }

  return mk_num((x.intv % y.intv));
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

expr op_is_procedure(struct machine *m, expr args)
{
  expr proc = car(args);
  return ((is_procedure(proc)) || (is_prim(proc)) || (is_continuation(proc))) ? TRUE : FALSE;
}

expr op_is_vector(struct machine *m, expr args)
{
  return is_vector(car(args)) ? TRUE : FALSE;
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

  return pattern_depths(m, pattern, literals);
}

expr op_environment(struct machine *m, expr args)
{
  return machine_get_reg(m, ENV);
}

expr op_machine_snapshot(struct machine *m, expr args)
{
  return machine_snapshot(m);
}

expr op_machine_restore(struct machine *m, expr args)
{
  expr snapshot = car(args);
  expr val = cadr(args);
  if (machine_restore(m, snapshot))
  {
    logexpr("Restored machine snapshot with value", val);
    machine_set_reg(m, VAL, val);
    return TRUE;
  }
  return FALSE;
}

expr op_exit(struct machine *m, expr args)
{
  expr code = car(args);
  machine_push(m, make_cont(NULL));
  logexpr("Exiting with code", code);
  return mk_exit(m);
}

expr op_define(struct machine *m, expr args)
{
  expr var = car(args);
  expr val = cadr(args);
  expr env = machine_get_reg(m, ENV);
  logexpr("Defining variable", var);
  logexpr("With value", val);
  env_define_variable(m, var, val, env);
  return TRUE;
}

expr primitives_env(struct machine *m)
{
  struct
  {
    const char *name;
    expr (*fn)(struct machine *, expr);
  } prims[] = {
      {"+", op_add},
      {"*", op_mul},
      {"-", op_sub},
      {"eq?", op_eq},
      {"eqv?", op_eq},
      {">", op_gt},
      {"<", op_lt},
      {"null?", op_is_null},
      {"number?", op_is_number},
      {"string?", op_is_string},
      {"pair?", op_is_pair},
      {"symbol?", op_is_symbol},
      {"procedure?", op_is_procedure},
      {"vector?", op_is_vector},
      {"mod", op_mod},
      {"make-vector", op_vector_create},
      {"vector-length", op_vector_size},
      {"vector-ref", op_vector_get},
      {"vector-set!", op_vector_set},
      {"car", op_car},
      {"cdr", op_cdr},
      {"cons", op_cons},
      {"list", op_list},
      {"list-ref", op_list_ref},
      {"memq", op_memq},
      {"display", op_print},
      {"newline", op_println},
      {"println", op_println},
      {"env", op_environment},
      {"machine-snapshot", op_machine_snapshot},
      {"machine-restore", op_machine_restore},
      {"exit", op_exit},
      {"define", op_define}, // Handled specially in evaluator
      {NULL, NULL}};
  expr frame = NIL;
  for (int i = 0; prims[i].name != NULL; i++)
  {
    expr binding = cons(m, mk_sym(prims[i].name), mk_prim(prims[i].fn));
    frame = cons(m, binding, frame);
  }

  return env_extend_with_frame(m, frame, NIL);
}
