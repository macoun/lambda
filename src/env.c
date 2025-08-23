//
//  env.c
//  Lisper
//
//  Created by Ferhat Ayaz on 14/03/2016.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#include "env.h"
#include "printer.h"

extern int is_equal(expr a, expr b);

static expr make_frame(expr vars, expr vals);

static void frame_bind(expr frame, expr var, expr val);
static expr frame_vars(expr frame);
static expr frame_vals(expr frame);

void print_frame(expr frame)
{
  expr vars, vals;

  vars = frame_vars(frame);
  vals = frame_vals(frame);

  while (!is_nil(vars))
  {
    print_exp(car(vars));
    printf(":\t");
    print_exp(car(vals));
    printf("\t\t*\n");

    vars = cdr(vars);
    vals = cdr(vals);
  }
}

expr *env_lookup(expr var, expr env)
{
  expr frame;
  expr *res;

  while (!is_nil(env))
  {
    frame = env_frame(env);
    res = frame_lookup(frame, var);
    if (res != NULL)
      return res;

    env = env_parent(env);
  }

  return NULL;
}

expr env_extend(expr vars, expr vals, expr env)
{
  expr frame;

  push(env);
  frame = make_frame(vars, vals);
  pop(env);
  return cons(frame, env);
}

expr env_define_variable(expr var, expr val, expr env)
{
  expr frame;
  expr *res;

  frame = env_frame(env);
  res = frame_lookup(frame, var);
  if (res != NULL)
    res[0] = val;
  else
    frame_bind(frame, var, val);

  return mk_num(1);
}

expr env_frame(expr env)
{
  return car(env);
}

expr env_parent(expr env)
{
  return cdr(env);
}

// Frame
expr make_frame(expr vars, expr vals)
{
  return cons(vars, vals);
}

expr frame_vars(expr frame)
{
  return car(frame);
}

expr frame_vals(expr frame)
{
  return cdr(frame);
}

expr *frame_lookup(expr frame, expr var)
{
  expr vars, vals;

  //  printf("\nLookup for %s\n", var.str);
  //  print_frame(frame);

  vars = frame_vars(frame);
  vals = frame_vals(frame);

  while (!is_nil(vars))
  {
    if (is_equal(car(vars), var))
      return vals.array;

    vars = cdr(vars);
    vals = cdr(vals);
  }
  return NULL;
}

static void frame_bind(expr frame, expr var, expr val)
{
  expr exp;

  push(val);
  push(frame);
  exp = cons(var, car(frame));
  pop(frame);
  pop(val);

  set_car(frame, exp);

  push(frame);
  exp = cons(val, cdr(frame));
  pop(frame);

  set_cdr(frame, exp);
}
