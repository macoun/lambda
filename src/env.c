//
//  env.c
//  Lisper
//
//  Created by Ferhat Ayaz on 14/03/2016.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#include "env.h"
#include "printer.h"
#include "logger.h"
#include "evaluator.h"

static expr make_frame(struct machine *m, expr vars, expr vals);
static void frame_bind(struct machine *m, expr frame, expr var, expr val);
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

expr env_extend(struct machine *m, expr vars, expr vals, expr env)
{
  expr frame;
  // info("Extending environment vars:");
  // print_exp(vars);
  // printf("\n");
  // info("with vals:");
  // print_exp(vals);
  // printf("\n");
  // fflush(stdout);
  machine_push(m, env);
  machine_push(m, vars);
  machine_push(m, vals);
  frame = make_frame(m, vars, vals);
  expr res = cons(m, frame, env);
  machine_pop(m, &vals);
  machine_pop(m, &vars);
  machine_pop(m, &env);
  return res;
}

expr env_define_variable(struct machine *m, expr var, expr val, expr env)
{
  expr frame;
  expr *res;

  frame = env_frame(env);
  res = frame_lookup(frame, var);
  if (res != NULL)
    res[0] = val;
  else
    frame_bind(m, frame, var, val);

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
expr make_frame(struct machine *m, expr vars, expr vals)
{
  return cons(m, vars, vals);
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

static void frame_bind(struct machine *m, expr frame, expr var, expr val)
{
  expr exp;

  machine_push(m, val);
  machine_push(m, frame);
  exp = cons(m, var, car(frame));
  machine_pop(m, &frame);
  machine_pop(m, &val);

  set_car(frame, exp);

  machine_push(m, frame);
  exp = cons(m, val, cdr(frame));
  machine_pop(m, &frame);

  set_cdr(frame, exp);
}