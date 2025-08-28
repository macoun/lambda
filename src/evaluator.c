//
//  evaluator.c
//  Lisper
//
//  Created by Ferhat Ayaz on 09/03/2016.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#include "evaluator.h"
#include "reader.h"
#include "printer.h"
#include "mem.h"
#include "env.h"
#include "logger.h"
#include "primitives.h"
#include "array.h"
#include "machine.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>

const long DEFAULT_STACK_SIZE = 1024 * 1;

#define make_cont(f) mk_cell(CUSTOM, f)

enum
{
  EXP,
  ENV,
  UNEV,
  ARGL,
  PROC,
  CONTINUE,
  VAL
};

static void eval_dispatch(struct evaluator *ev);
static void ev_appl_did_operator(struct evaluator *ev);
static void ev_appl_operand_loop(struct evaluator *ev);
static void ev_appl_accum_arg(struct evaluator *ev);
static void ev_appl_accum_last_arg(struct evaluator *ev);
static void ev_appl_last_arg(struct evaluator *ev);
static void ev_begin(struct evaluator *ev);
static void ev_sequence(struct evaluator *ev);
static void ev_sequence_cont(struct evaluator *ev);
static void ev_sequence_end(struct evaluator *ev);
static void ev_definition(struct evaluator *ev);
static void ev_definition_cont(struct evaluator *ev);
static void ev_application(struct evaluator *ev);
static void ev_lambda(struct evaluator *ev);
static void ev_quote(struct evaluator *ev);
static void ev_variable(struct evaluator *ev);
static void ev_self(struct evaluator *ev);
static void ev_if(struct evaluator *ev);
static void ev_if_alternative(struct evaluator *ev);
static void ev_if_consequent(struct evaluator *ev);
static void ev_if_decide(struct evaluator *ev);

static void apply_dispatch(struct evaluator *ev);
static void primitive_apply(struct evaluator *ev);
static void compound_apply(struct evaluator *ev);

static expr adjoin_arg(struct machine *m, expr val, expr argl);
static int primitives_init(struct evaluator *ev);

static inline void push_reg(struct evaluator *m, int reg)
{
  machine_push_reg(m->machine, reg);
}

static inline void pop_reg(struct evaluator *m, int reg)
{
  machine_pop_reg(m->machine, reg);
}

static inline void mov_reg(struct evaluator *m, int srcreg, int destreg)
{
  machine_mov_reg(m->machine, srcreg, destreg);
}

static inline void set_reg(struct evaluator *m, int reg, struct cell val)
{
  machine_set_reg(m->machine, reg, val);
}

static inline struct cell get_reg(struct evaluator *m, int reg)
{
  return machine_get_reg(m->machine, reg);
}

struct evaluator *evaluator_create()
{
  struct evaluator *ev = (struct evaluator *)malloc(sizeof(struct evaluator));
  if (!ev)
    return NULL;

  ev->memory = memory_create();
  if (!ev->memory)
  {
    fprintf(stderr, "Cannot create memory manager\n");
    free(ev);
    return NULL;
  }
  ev->machine = machine_create(ev->memory, 7, DEFAULT_STACK_SIZE);
  if (!ev->machine)
  {
    fprintf(stderr, "Cannot create machine\n");
    memory_destroy(ev->memory);
    free(ev);
    return NULL;
  }
  fprintf(stdout, "Created machine with stack size %ld\n", DEFAULT_STACK_SIZE);
  primitives_init(ev);
  fprintf(stdout, "Initialized evaluator\n");
  return ev;
}

void evaluator_destroy(struct evaluator *ev)
{
  if (ev)
  {
    machine_destroy(ev->machine);
    memory_destroy(ev->memory);
    free(ev);
  }
}

#pragma mark Initialization

static int primitives_init(struct evaluator *ev)
{
  expr vars, vals;

  vars = listn(ev->machine, 27,
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
               mk_sym("display"),
               mk_sym("newline"),
               mk_sym("println"),
               NIL);
  vals = listn(ev->machine, 27,
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
               mk_prim(op_print),
               mk_prim(op_println),
               mk_prim(op_println),
               NIL);
  set_reg(ev, ENV, env_extend(ev->machine, vars, vals, NIL));

  return 1;
}

void add_primitives(struct machine *m, expr names, expr funcs)
{
  machine_set_reg(m, ENV, env_extend(m, names, funcs, machine_get_reg(m, ENV)));
}

expr *lookup_name(struct machine *m, const char *name)
{
  return env_lookup(mk_sym(name), machine_get_reg(m, ENV));
}

#pragma mark Evaluator

expr eval(struct evaluator *ev, expr exp)
{
  expr val;

  set_reg(ev, EXP, exp);
  set_reg(ev, CONTINUE, make_cont(NULL));
  ev->goto_fn = eval_dispatch;

  while (ev->goto_fn != NULL)
  {
    ((cont_f)ev->goto_fn)(ev);
  }
  val = get_reg(ev, VAL);
  return val;
}

static void eval_dispatch(struct evaluator *ev)
{
  expr exp;

  exp = get_reg(ev, EXP);
  if (is_self_eval(exp))
  {
    ev_self(ev);
  }
  else if (is_variable(exp))
  {
    ev_variable(ev);
  }
  else if (is_if(exp))
  {
    ev_if(ev);
  }
  else if (is_quote(exp))
  {
    ev_quote(ev);
  }
  else if (is_lambda(exp))
  {
    ev_lambda(ev);
  }
  else if (is_definition(exp))
  {
    ev_definition(ev);
  }
  else if (is_begin(exp))
  {
    ev_begin(ev);
  }
  // Check appl as last condition
  else if (is_application(exp))
  {
    ev_application(ev);
  }
  else
  {
    error("Unknown expression %d", exp.type);
    exit(1);
  }
}

static void ev_self(struct evaluator *ev)
{
  mov_reg(ev, EXP, VAL);
  ev->goto_fn = (cont_f)get_reg(ev, CONTINUE).value;
}

static void ev_quote(struct evaluator *ev)
{
  expr exp = get_reg(ev, EXP);
  set_reg(ev, VAL, quote_text(exp));
  ev->goto_fn = (cont_f)get_reg(ev, CONTINUE).value;
}

static void ev_variable(struct evaluator *ev)
{
  expr *p;
  expr exp, env;
  exp = get_reg(ev, EXP);
  env = get_reg(ev, ENV);
  p = env_lookup(exp, env);
  if (p == NULL)
  {
    error("Unbound variable %s", exp.str);
    set_reg(ev, VAL, NIL);
    ev->goto_fn = NULL;
  }
  else
  {
    set_reg(ev, VAL, *p);
    ev->goto_fn = (cont_f)get_reg(ev, CONTINUE).value;
  }
}

static void ev_lambda(struct evaluator *ev)
{
  expr exp, unev, env, val;
  exp = get_reg(ev, EXP);
  env = get_reg(ev, ENV);
  unev = lambda_params(exp);
  exp = lambda_body(exp);
  val = make_procedure(ev->machine, unev, exp, env);

  set_reg(ev, UNEV, unev);
  set_reg(ev, EXP, exp);
  set_reg(ev, VAL, val);

  ev->goto_fn = (cont_f)get_reg(ev, CONTINUE).value;
}

static void ev_application(struct evaluator *ev)
{
  push_reg(ev, CONTINUE);
  push_reg(ev, ENV);
  set_reg(ev, UNEV, appl_operands(get_reg(ev, EXP)));
  push_reg(ev, UNEV);
  set_reg(ev, EXP, appl_operator(get_reg(ev, EXP)));
  set_reg(ev, CONTINUE, make_cont(ev_appl_did_operator));
  ev->goto_fn = eval_dispatch;
}

static void ev_appl_did_operator(struct evaluator *ev)
{
  pop_reg(ev, UNEV);
  pop_reg(ev, ENV);
  set_reg(ev, ARGL, NIL);
  mov_reg(ev, VAL, PROC);
  if (is_nil(get_reg(ev, UNEV)))
  {
    ev->goto_fn = apply_dispatch;
  }
  else
  {
    ev->goto_fn = ev_appl_operand_loop;
    push_reg(ev, PROC);
  }
}

static void ev_appl_operand_loop(struct evaluator *ev)
{
  push_reg(ev, ARGL);
  set_reg(ev, EXP, operands_first(get_reg(ev, UNEV)));
  if (operands_is_last(get_reg(ev, UNEV)))
  {
    ev->goto_fn = ev_appl_last_arg;
  }
  else
  {
    push_reg(ev, ENV);
    push_reg(ev, UNEV);
    set_reg(ev, CONTINUE, make_cont(ev_appl_accum_arg));
    ev->goto_fn = eval_dispatch;
  }
}
static void ev_appl_accum_arg(struct evaluator *ev)
{
  pop_reg(ev, UNEV);
  pop_reg(ev, ENV);
  pop_reg(ev, ARGL);
  set_reg(ev, ARGL, adjoin_arg(ev->machine, get_reg(ev, VAL), get_reg(ev, ARGL)));
  set_reg(ev, UNEV, operands_rest(get_reg(ev, UNEV)));

  ev->goto_fn = ev_appl_operand_loop;
}

static void ev_appl_last_arg(struct evaluator *ev)
{
  set_reg(ev, CONTINUE, make_cont(ev_appl_accum_last_arg));

  ev->goto_fn = eval_dispatch;
}

static void ev_appl_accum_last_arg(struct evaluator *ev)
{
  pop_reg(ev, ARGL);
  set_reg(ev, ARGL, adjoin_arg(ev->machine, get_reg(ev, VAL), get_reg(ev, ARGL)));
  pop_reg(ev, PROC);
  ev->goto_fn = apply_dispatch;
}

static void ev_begin(struct evaluator *ev)
{
  set_reg(ev, UNEV, begin_actions(get_reg(ev, EXP)));
  push_reg(ev, CONTINUE);
  ev->goto_fn = ev_sequence;
}

#define TAIL_RECURSIVE 1

#if TAIL_RECURSIVE

static void ev_sequence(struct evaluator *ev)
{
  set_reg(ev, EXP, car(get_reg(ev, UNEV))); // Next expression
  if (is_nil(cdr(get_reg(ev, UNEV))))       // If last expression
  {
    ev->goto_fn = ev_sequence_end;
  }
  else
  {
    push_reg(ev, UNEV);
    push_reg(ev, ENV);
    set_reg(ev, CONTINUE, make_cont(ev_sequence_cont));

    ev->goto_fn = eval_dispatch;
  }
}

static void ev_sequence_end(struct evaluator *ev)
{
  pop_reg(ev, CONTINUE);
  ev->goto_fn = eval_dispatch;
}

#else

static void ev_sequence(struct evaluator *ev)
{
  if (is_nil(get_reg(ev, UNEV))) // If last expression
  {
    ev->goto_fn = ev_sequence_end;
  }
  else
  {
    set_reg(ev, EXP, car(get_reg(ev, UNEV))); // Next expression
    push_reg(ev, UNEV);
    push_reg(ev, ENV);
    set_reg(ev, CONTINUE, make_cont(ev_sequence_cont));

    ev->goto_fn = eval_dispatch;
  }
}

static void ev_sequence_end(struct evaluator *ev)
{
  pop_reg(ev, CONTINUE);
  ev->goto_fn = (cont_f)get_reg(ev, CONTINUE).value;
}

#endif

static void ev_sequence_cont(struct evaluator *ev)
{
  pop_reg(ev, ENV);
  pop_reg(ev, UNEV);
  set_reg(ev, UNEV, cdr(get_reg(ev, UNEV))); // Rest expression

  ev->goto_fn = ev_sequence;
}

static void apply_dispatch(struct evaluator *ev)
{
  if (is_prim(get_reg(ev, PROC)))
  {
    ev->goto_fn = primitive_apply;
  }
  else if (is_procedure(get_reg(ev, PROC)))
  {
    ev->goto_fn = compound_apply;
  }
  else
  {
    error("Unknown procedure");
    ev->goto_fn = NULL;
  }
}

static void primitive_apply(struct evaluator *ev)
{
  primitive_f fn = (primitive_f)get_reg(ev, PROC).value;
  expr val = fn(ev->machine, get_reg(ev, ARGL));
  set_reg(ev, VAL, val);
  pop_reg(ev, CONTINUE);
  ev->goto_fn = (cont_f)get_reg(ev, CONTINUE).value;
}

static void compound_apply(struct evaluator *ev)
{
  set_reg(ev, UNEV, procedure_params(get_reg(ev, PROC)));
  set_reg(ev, ENV, procedure_env(get_reg(ev, PROC)));
  set_reg(ev, ENV, env_extend(ev->machine, get_reg(ev, UNEV), get_reg(ev, ARGL), get_reg(ev, ENV)));
  set_reg(ev, UNEV, procedure_body(get_reg(ev, PROC)));

  ev->goto_fn = ev_sequence;
}

static void ev_definition(struct evaluator *ev)
{
  set_reg(ev, UNEV, definition_variable(get_reg(ev, EXP)));
  push_reg(ev, UNEV);
  set_reg(ev, EXP, definition_value(ev->machine, get_reg(ev, EXP)));
  push_reg(ev, ENV);
  push_reg(ev, CONTINUE);
  set_reg(ev, CONTINUE, make_cont(ev_definition_cont));

  ev->goto_fn = eval_dispatch;
}

static void ev_definition_cont(struct evaluator *ev)
{
  pop_reg(ev, CONTINUE);
  pop_reg(ev, ENV);
  pop_reg(ev, UNEV);
  env_define_variable(ev->machine, get_reg(ev, UNEV), get_reg(ev, VAL), get_reg(ev, ENV));
  set_reg(ev, VAL, mk_num(1));
  ev->goto_fn = (cont_f)get_reg(ev, CONTINUE).value;
}

static void ev_if(struct evaluator *ev)
{
  push_reg(ev, EXP);
  push_reg(ev, ENV);
  push_reg(ev, CONTINUE);
  set_reg(ev, CONTINUE, make_cont(ev_if_decide));
  set_reg(ev, EXP, if_predicate(get_reg(ev, EXP)));

  ev->goto_fn = eval_dispatch;
}

static void ev_if_decide(struct evaluator *ev)
{
  pop_reg(ev, CONTINUE);
  pop_reg(ev, ENV);
  pop_reg(ev, EXP);

  if (get_reg(ev, VAL).intv != 0)
    ev->goto_fn = ev_if_consequent;
  else
    ev->goto_fn = ev_if_alternative;
}

static void ev_if_alternative(struct evaluator *ev)
{
  set_reg(ev, EXP, if_alternative(get_reg(ev, EXP)));
  ev->goto_fn = eval_dispatch;
}

static void ev_if_consequent(struct evaluator *ev)
{
  set_reg(ev, EXP, if_consequent(get_reg(ev, EXP)));
  ev->goto_fn = eval_dispatch;
}

int is_equal(expr a, expr b)
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
  }
  return 0;
}

static expr adjoin_arg(struct machine *m, expr val, expr argl)
{
  return list_append(m, argl, val);
}
