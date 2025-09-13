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
static expr frame_bind(struct machine *m, expr frame, expr var, expr val);
static expr *frame_lookup(expr frame, expr var);

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

  machine_push(m, env);
  frame = make_frame(m, vars, vals);
  machine_pop(m, &env);

  return cons(m, frame, env);
}

expr env_define_variable(struct machine *m, expr var, expr val, expr env)
{
  expr frame;

  frame = env_frame(env);
  expr bind = assoq(var, frame);

  if (!is_false(bind))
  {
    set_cdr(bind, val);
  }
  else
  {
    machine_push(m, env);
    bind = cons(m, var, val);
    machine_pop(m, &env);
    set_car(env, cons(m, bind, env_frame(env)));
  }

  return TRUE;
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
static expr make_frame(struct machine *m, expr vars, expr vals)
{
  expr frame = NIL;

  while (is_pair(vars) && is_pair(vals))
  {
    if (!is_sym(car(vars)))
    {
      error("Non-symbol in variable list");
      print_exp(car(vars));
      printf("\n");
      return FALSE;
    }

    machine_push(m, vars);
    machine_push(m, vals);
    frame = frame_bind(m, frame, car(vars), car(vals));
    machine_pop(m, &vals);
    machine_pop(m, &vars);

    vars = cdr(vars);
    vals = cdr(vals);
  }
  return frame;
}

static expr *frame_lookup(expr frame, expr var)
{
  expr bind = assoq(var, frame);
  if (!is_false(bind))
    return bind.array + 1;
  return NULL;
}

static expr frame_bind(struct machine *m, expr frame, expr var, expr val)
{
  machine_push(m, frame);
  expr bind = cons(m, var, val);
  machine_pop(m, &frame);

  return cons(m, bind, frame);
}
