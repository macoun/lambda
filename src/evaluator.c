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

#define __exp ev->machine->registers->cells[EXP]
#define __env ev->machine->registers->cells[ENV]
#define __unev ev->machine->registers->cells[UNEV]
#define __argl ev->machine->registers->cells[ARGL]
#define __proc ev->machine->registers->cells[PROC]
#define __continue ev->machine->registers->cells[CONTINUE]
#define __val ev->machine->registers->cells[VAL]

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
  __env = env_extend(ev->machine, vars, vals, NIL);

  return 1;
}

void add_primitives(struct machine *m, expr names, expr funcs)
{
  m->registers->cells[ENV] = env_extend(m, names, funcs, m->registers->cells[ENV]);
}

expr *lookup_name(struct machine *m, const char *name)
{
  return env_lookup(mk_sym(name), m->registers->cells[ENV]);
}

#pragma mark Evaluator

expr eval(struct evaluator *ev, expr exp)
{
  expr val;

  __exp = exp;
  __continue = make_cont(NULL);
  ev->goto_fn = eval_dispatch;

  while (ev->goto_fn != NULL)
  {
    ((cont_f)ev->goto_fn)(ev);
  }
  val = __val;
  return val;
}

static void eval_dispatch(struct evaluator *ev)
{

  if (is_self_eval(__exp))
  {
    ev_self(ev);
  }
  else if (is_variable(__exp))
  {
    ev_variable(ev);
  }
  else if (is_if(__exp))
  {
    ev_if(ev);
  }
  else if (is_quote(__exp))
  {
    ev_quote(ev);
  }
  else if (is_lambda(__exp))
  {
    ev_lambda(ev);
  }
  else if (is_definition(__exp))
  {
    ev_definition(ev);
  }
  else if (is_begin(__exp))
  {
    ev_begin(ev);
  }
  // Check appl as last condition
  else if (is_application(__exp))
  {
    ev_application(ev);
  }
  else
  {
    error("Unknown expression %d", __exp.type);
    exit(1);
  }
}

static void ev_self(struct evaluator *ev)
{
  __val = __exp;
  ev->goto_fn = (cont_f)__continue.value;
}

static void ev_quote(struct evaluator *ev)
{
  __val = quote_text(__exp);
  ev->goto_fn = (cont_f)__continue.value;
}

static void ev_variable(struct evaluator *ev)
{
  expr *p;
  p = env_lookup(__exp, __env);
  if (p == NULL)
  {
    error("Unbound variable %s", __exp.str);
    __val = NIL;
    ev->goto_fn = NULL;
  }
  else
  {
    __val = *p;
    ev->goto_fn = (cont_f)__continue.value;
  }
}

static void ev_lambda(struct evaluator *ev)
{
  __unev = lambda_params(__exp);
  __exp = lambda_body(__exp);
  __val = make_procedure(ev->machine, __unev, __exp, __env);
  ev->goto_fn = (cont_f)__continue.value;
}

static void ev_application(struct evaluator *ev)
{
  push(__continue);
  push(__env);
  __unev = appl_operands(__exp);
  push(__unev);
  __exp = appl_operator(__exp);
  __continue = make_cont(ev_appl_did_operator);
  ev->goto_fn = eval_dispatch;
}

static void ev_appl_did_operator(struct evaluator *ev)
{
  pop(__unev);
  pop(__env);
  __argl = NIL;
  __proc = __val;
  if (is_nil(__unev))
  {
    ev->goto_fn = apply_dispatch;
  }
  else
  {
    ev->goto_fn = ev_appl_operand_loop;
    push(__proc);
  }
}

static void ev_appl_operand_loop(struct evaluator *ev)
{
  push(__argl);
  __exp = operands_first(__unev);
  if (operands_is_last(__unev))
  {
    ev->goto_fn = ev_appl_last_arg;
  }
  else
  {
    push(__env);
    push(__unev);
    __continue = make_cont(ev_appl_accum_arg);
    ev->goto_fn = eval_dispatch;
  }
}
static void ev_appl_accum_arg(struct evaluator *ev)
{
  pop(__unev);
  pop(__env);
  pop(__argl);
  __argl = adjoin_arg(ev->machine, __val, __argl);
  __unev = operands_rest(__unev);
  ev->goto_fn = ev_appl_operand_loop;
}

static void ev_appl_last_arg(struct evaluator *ev)
{
  __continue = make_cont(ev_appl_accum_last_arg);
  ev->goto_fn = eval_dispatch;
}

static void ev_appl_accum_last_arg(struct evaluator *ev)
{
  pop(__argl);
  __argl = adjoin_arg(ev->machine, __val, __argl);
  pop(__proc);
  ev->goto_fn = apply_dispatch;
}

static void ev_begin(struct evaluator *ev)
{
  __unev = begin_actions(__exp);
  push(__continue);
  ev->goto_fn = ev_sequence;
}

#define TAIL_RECURSIVE 1

#if TAIL_RECURSIVE

static void ev_sequence(struct evaluator *ev)
{
  __exp = car(__unev);     // Next expression
  if (is_nil(cdr(__unev))) // If last expression
  {
    ev->goto_fn = ev_sequence_end;
  }
  else
  {
    push(__unev);
    push(__env);
    __continue = make_cont(ev_sequence_cont);
    ev->goto_fn = eval_dispatch;
  }
}

static void ev_sequence_end(struct evaluator *ev)
{
  pop(__continue);
  ev->goto_fn = eval_dispatch;
}

#else

static void ev_sequence(struct evaluator *ev)
{
  if (is_nil(__unev)) // If last expression
  {
    ev->goto_fn = ev_sequence_end;
  }
  else
  {
    __exp = car(__unev); // Next expression
    push(__unev);
    push(__env);
    __continue = make_cont(ev_sequence_cont);
    ev->goto_fn = eval_dispatch;
  }
}

static void ev_sequence_end(struct evaluator *ev)
{
  pop(__continue);
  ev->goto_fn = (cont_f)__continue.value;
}

#endif

static void ev_sequence_cont(struct evaluator *ev)
{
  pop(__env);
  pop(__unev);
  __unev = cdr(__unev); // Rest expression
  ev->goto_fn = ev_sequence;
}

static void apply_dispatch(struct evaluator *ev)
{
  if (is_prim(__proc))
  {
    ev->goto_fn = primitive_apply;
  }
  else if (is_procedure(__proc))
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
  __val = ((primitive_f)__proc.value)(ev->machine, __argl);
  pop(__continue);
  ev->goto_fn = (cont_f)__continue.value;
}

static void compound_apply(struct evaluator *ev)
{
  __unev = procedure_params(__proc);
  __env = procedure_env(__proc);
  __env = env_extend(ev->machine, __unev, __argl, __env);
  __unev = procedure_body(__proc);
  ev->goto_fn = ev_sequence;
}

static void ev_definition(struct evaluator *ev)
{
  __unev = definition_variable(__exp);
  push(__unev);
  __exp = definition_value(ev->machine, __exp);
  push(__env);
  push(__continue);
  __continue = make_cont(ev_definition_cont);
  ev->goto_fn = eval_dispatch;
}

static void ev_definition_cont(struct evaluator *ev)
{
  pop(__continue);
  pop(__env);
  pop(__unev);
  env_define_variable(ev->machine, __unev, __val, __env);
  __val = mk_num(1);
  ev->goto_fn = (cont_f)__continue.value;
}

static void ev_if(struct evaluator *ev)
{
  push(__exp);
  push(__env);
  push(__continue);
  __continue = make_cont(ev_if_decide);
  __exp = if_predicate(__exp);
  ev->goto_fn = eval_dispatch;
}

static void ev_if_decide(struct evaluator *ev)
{
  pop(__continue);
  pop(__env);
  pop(__exp);

  if (__val.intv != 0)
    ev->goto_fn = ev_if_consequent;
  else
    ev->goto_fn = ev_if_alternative;
}

static void ev_if_alternative(struct evaluator *ev)
{
  __exp = if_alternative(__exp);
  ev->goto_fn = eval_dispatch;
}

static void ev_if_consequent(struct evaluator *ev)
{
  __exp = if_consequent(__exp);
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
