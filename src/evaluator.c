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
#include <assert.h>
#include <stdarg.h>

// entry point for evaluation
// dispatch to the appropriate evaluation function
static void eval_dispatch(struct evaluator *ev);

// (lambda (params ...) body ...)
static void ev_lambda(struct evaluator *ev);
static void ev_sequence(struct evaluator *ev);
static void ev_sequence_cont(struct evaluator *ev);
static void ev_sequence_end(struct evaluator *ev);

// (define var exp) or (define (func args ...) body ...)
static void ev_definition(struct evaluator *ev);
static void ev_definition_cont(struct evaluator *ev);

// (proc arg ...)
static void ev_application(struct evaluator *ev);
static void ev_appl_did_operator(struct evaluator *ev);
static void ev_appl_operand_loop(struct evaluator *ev);
static void ev_appl_accum_arg(struct evaluator *ev);
static void ev_appl_accum_last_arg(struct evaluator *ev);
static void ev_appl_last_arg(struct evaluator *ev);

// (apply proc arg ...)
static void ev_apply(struct evaluator *ev);

// (quote exp)
static void ev_quote(struct evaluator *ev);

// identifier
static void ev_variable(struct evaluator *ev);

// number, string, symbol, true, false, nil
static void ev_self(struct evaluator *ev);

// (if test conseq alt)
static void ev_if(struct evaluator *ev);
static void ev_if_decide(struct evaluator *ev);
static void ev_if_consequent(struct evaluator *ev);
static void ev_if_alternative(struct evaluator *ev);

// (call/cc proc)
static void ev_callcc(struct evaluator *ev);
static void ev_callcc_did_proc(struct evaluator *ev);

// apply dispatch
static void apply_dispatch(struct evaluator *ev);
static void primitive_apply(struct evaluator *ev);
static void compound_apply(struct evaluator *ev);
static void continuation_apply(struct evaluator *ev);

static expr adjoin_arg(struct machine *m, expr val, expr argl);

char *eval_func_name(cont_f f);

inline void push_reg(struct evaluator *m, int reg)
{
  machine_push_reg(m->machine, reg);
}

inline void pop_reg(struct evaluator *m, int reg)
{
  machine_pop_reg(m->machine, reg);
}

inline void mov_reg(struct evaluator *m, int srcreg, int destreg)
{
  machine_mov_reg(m->machine, srcreg, destreg);
}

inline void set_reg(struct evaluator *m, int reg, struct cell val)
{
  machine_set_reg(m->machine, reg, val);
}

inline struct cell get_reg(struct evaluator *m, int reg)
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

  set_reg(ev, ENV, primitives_env(ev->machine));

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

#pragma mark Evaluator

expr eval(struct evaluator *ev, expr exp)
{
  expr val;

  // Test stack size after evaluation
  long sp = ev->machine->stack->size;

  set_reg(ev, EXP, exp);
  set_reg(ev, CONTINUE, make_cont(NULL));
  ev->goto_fn = eval_dispatch;

  while (ev->goto_fn != NULL)
  {
    // printf("-> %s\n", eval_func_name((cont_f)ev->goto_fn));
    // logexpr("EXP", get_reg(ev, EXP));
    ((cont_f)ev->goto_fn)(ev);
  }

  val = get_reg(ev, VAL);
  if (is_exit(val))
  {
    logexpr("Exiting evaluator with code", val);
  }
  else
  {
    // printf("Final stack size: %zu (was %ld)\n", ev->machine->stack->size, sp);
    assert(ev->machine->stack->size == sp);
  }

  return val;
}

static void eval_dispatch(struct evaluator *ev)
{
  expr exp;

  exp = get_reg(ev, EXP);

  if (is_self_eval(exp))
  {
    ev->goto_fn = (cont_f)ev_self;
  }
  else if (is_variable(exp))
  {
    ev->goto_fn = (cont_f)ev_variable;
  }
  else if (is_if(exp))
  {
    ev->goto_fn = (cont_f)ev_if;
  }
  else if (is_quote(exp))
  {
    ev->goto_fn = (cont_f)ev_quote;
  }
  else if (is_lambda(exp))
  {
    ev->goto_fn = (cont_f)ev_lambda;
  }
  else if (is_definition(exp) || is_assignment(exp))
  {
    ev->goto_fn = (cont_f)ev_definition;
  }
  else if (is_callcc(exp))
  {
    ev->goto_fn = (cont_f)ev_callcc;
  }
  else if (is_apply(exp))
  {
    ev->goto_fn = (cont_f)ev_apply;
  }
  // Check appl as last condition
  else if (is_application(exp))
  {
    ev->goto_fn = (cont_f)ev_application;
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
  set_reg(ev, VAL, quote_text(get_reg(ev, EXP)));
  ev->goto_fn = (cont_f)get_reg(ev, CONTINUE).value;
}

static void ev_variable(struct evaluator *ev)
{
  expr bind = FALSE;
  if (is_scoped_sym(get_reg(ev, EXP)))
  {
    bind = get_reg(ev, EXP).array[1];
  }
  else
  {
    bind = env_lookup(get_reg(ev, EXP), get_reg(ev, ENV));
  }
  if (is_false(bind))
  {
    error("Unbound variable %s", get_reg(ev, EXP).str);
    set_reg(ev, VAL, NIL);
    // ev->goto_fn = NULL; // TODO handle stack unwinding
    ev->goto_fn = (cont_f)get_reg(ev, CONTINUE).value;
  }
  else
  {
    set_reg(ev, VAL, cdr(bind));
    ev->goto_fn = (cont_f)get_reg(ev, CONTINUE).value;
  }
}

static void ev_callcc(struct evaluator *ev)
{
  // Capture current continuation
  set_reg(ev, UNEV, machine_snapshot(ev->machine));
  set_reg(ev, UNEV, make_continuation(ev->machine, get_reg(ev, UNEV)));
  push_reg(ev, CONTINUE);
  push_reg(ev, ENV);
  push_reg(ev, UNEV);
  set_reg(ev, EXP, callcc_proc(get_reg(ev, EXP)));
  set_reg(ev, CONTINUE, make_cont(ev_callcc_did_proc));

  ev->goto_fn = eval_dispatch;
}

static void ev_callcc_did_proc(struct evaluator *ev)
{
  pop_reg(ev, UNEV);
  pop_reg(ev, ENV);
  set_reg(ev, ARGL, cons(ev->machine, get_reg(ev, UNEV), NIL));
  mov_reg(ev, VAL, PROC);

  ev->goto_fn = apply_dispatch;
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
    push_reg(ev, PROC);
    ev->goto_fn = ev_appl_operand_loop;
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

static void ev_lambda(struct evaluator *ev)
{
  set_reg(ev, UNEV, lambda_params(get_reg(ev, EXP)));
  set_reg(ev, EXP, lambda_body(get_reg(ev, EXP)));
  set_reg(ev, VAL, make_procedure(ev->machine, get_reg(ev, UNEV), get_reg(ev, EXP), get_reg(ev, ENV)));

  ev->goto_fn = (cont_f)get_reg(ev, CONTINUE).value;
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

static void ev_sequence_cont(struct evaluator *ev)
{
  pop_reg(ev, ENV);
  pop_reg(ev, UNEV);
  set_reg(ev, UNEV, cdr(get_reg(ev, UNEV))); // Rest expression

  ev->goto_fn = ev_sequence;
}

static void ev_sequence_end(struct evaluator *ev)
{
  pop_reg(ev, CONTINUE);
  // printf("ev_sequence_end: continue %s\n", eval_func_name((cont_f)get_reg(ev, CONTINUE).value));
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
  else if (is_continuation(get_reg(ev, PROC)))
  {
    ev->goto_fn = continuation_apply;
  }
  else
  {
    error("Unknown procedure");
    print_exp(car(get_reg(ev, PROC)));
    printf("\n");
    ev->goto_fn = NULL; // TODO handle stack unwinding
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

static void continuation_apply(struct evaluator *ev)
{
  expr val = car(get_reg(ev, ARGL));

  set_reg(ev, PROC, continuation_snapshot(get_reg(ev, PROC)));
  machine_restore(ev->machine, get_reg(ev, PROC));
  set_reg(ev, VAL, val);

  ev->goto_fn = (cont_f)get_reg(ev, CONTINUE).value;
}

static void ev_apply(struct evaluator *ev)
{
  set_reg(ev, EXP, cdr(get_reg(ev, EXP))); // Skip 'apply
  ev->goto_fn = eval_dispatch;
}

static void ev_definition(struct evaluator *ev)
{
  set_reg(ev, UNEV, definition_variable(get_reg(ev, EXP)));
  push_reg(ev, UNEV);
  push_reg(ev, EXP);
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
  pop_reg(ev, EXP);
  pop_reg(ev, UNEV);

  if (is_assignment(get_reg(ev, EXP)))
  {
    set_reg(ev, VAL, env_set_variable(ev->machine, get_reg(ev, UNEV), get_reg(ev, VAL), get_reg(ev, ENV)));
  }
  else
  {
    set_reg(ev, VAL, env_define_variable(ev->machine, get_reg(ev, UNEV), get_reg(ev, VAL), get_reg(ev, ENV)));
  }

  if (is_false(get_reg(ev, VAL)))
    ev->goto_fn = (cont_f)get_reg(ev, CONTINUE).value;
  // ev->goto_fn = NULL; // TODO handle stack unwinding
  else
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

  if (!is_false(get_reg(ev, VAL)))
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

static expr adjoin_arg(struct machine *m, expr val, expr argl)
{
  machine_push(m, argl);
  expr rest = cons(m, val, NIL);
  machine_pop(m, &argl);
  return list_append_x(m, argl, rest);
}

// ----------------------------------------------------------------

struct eval_func
{
  const char *name;
  cont_f func;
} evfuncs[] = {
    {"eval_dispatch", eval_dispatch},
    {"ev_sequence", ev_sequence},
    {"ev_sequence_cont", ev_sequence_cont},
    {"ev_sequence_end", ev_sequence_end},
    {"ev_definition", ev_definition},
    {"ev_definition_cont", ev_definition_cont},
    {"ev_apply", ev_apply},
    {"ev_application", ev_application},
    {"ev_appl_did_operator", ev_appl_did_operator},
    {"ev_appl_operand_loop", ev_appl_operand_loop},
    {"ev_appl_accum_arg", ev_appl_accum_arg},
    {"ev_appl_accum_last_arg", ev_appl_accum_last_arg},
    {"ev_appl_last_arg", ev_appl_last_arg},
    {"ev_lambda", ev_lambda},
    {"ev_quote", ev_quote},
    {"ev_variable", ev_variable},
    {"ev_self", ev_self},
    {"ev_if", ev_if},
    {"ev_if_alternative", ev_if_alternative},
    {"ev_if_consequent", ev_if_consequent},
    {"ev_if_decide", ev_if_decide},
    {"ev_callcc", ev_callcc},
    {"ev_callcc_did_proc", ev_callcc_did_proc},
    {"apply_dispatch", apply_dispatch},
    {"primitive_apply", primitive_apply},
    {"compound_apply", compound_apply},
    {"continuation_apply", continuation_apply},
    {NULL, NULL}};

char *eval_func_name(cont_f f)
{
  if (f == NULL)
    return "NULL";
  struct eval_func *ef = evfuncs;
  while (ef->name != NULL)
  {
    if (ef->func == f)
      return (char *)ef->name;
    ef++;
  }
  return "unknown";
}
