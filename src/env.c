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
#include "match.h"

static expr make_frame(struct machine *m, expr vars, expr vals);
static expr frame_lookup(expr frame, expr var);

expr env_lookup(expr var, expr env)
{
  expr frame;
  expr res;

  while (!is_nil(env))
  {
    frame = env_frame(env);
    res = frame_lookup(frame, var);
    if (!is_false(res))
      return res;

    env = env_parent(env);
  }

  return FALSE;
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

  if (is_false(bind))
  {
    machine_push(m, env);
    bind = cons(m, var, val);
    machine_pop(m, &env);
    set_car(env, cons(m, bind, env_frame(env)));
    return TRUE;
  }

  error("Duplicate variable definition: %s", var.str);
  return FALSE;
}

expr env_set_variable(struct machine *m, expr var, expr val, expr env)
{
  expr bind = env_lookup(var, env);
  if (!is_false(bind))
  {
    set_cdr(bind, val);
    return TRUE;
  }
  error("Can not set unbound var %s", var.str);
  return FALSE;
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
  if (is_nil(vars) && is_nil(vals))
    return NIL;

  memory_enable_gc(m->memory, false);
  expr bindings = match_pattern(m, vars, vals, NIL);
  if (is_false(bindings))
  {
    error("Mismatch in number of variables and values");
    print_exp(vars);
    printf("\n");
    print_exp(vals);
    printf("\n");
  }
  memory_enable_gc(m->memory, true);
  return bindings;
}

static expr frame_lookup(expr frame, expr var)
{
  return assoq(var, frame);
}
