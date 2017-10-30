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
#include "primitives.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>

typedef void (*cont_f)(void);
#define make_cont(f) mk_cell(CUSTOM, f)

expr __exp;
expr __val;
expr __env;
expr __unev;
expr __argl;
expr __proc;
expr __continue;

expr *__registers[] =
{
  &__exp, &__val, &__env,
  &__unev, &__argl, &__proc,
  &__continue,
  NULL
};


static cont_f __goto;

static void eval_dispatch(void);
static void ev_appl_did_operator(void);
static void ev_appl_operand_loop(void);
static void ev_appl_accum_arg(void);
static void ev_appl_accum_last_arg(void);
static void ev_appl_last_arg(void);
static void ev_begin(void);
static void ev_sequence(void);
static void ev_sequence_cont(void);
static void ev_sequence_end(void);
static void ev_definition(void);
static void ev_definition_cont(void);
static void ev_application(void);
static void ev_lambda(void);
static void ev_quote(void);
static void ev_variable(void);
static void ev_self(void);
static void ev_if(void);
static void ev_if_alternative(void);
static void ev_if_consequent(void);
static void ev_if_decide(void);

static void apply_dispatch(void);
static void primitive_apply(void);
static void compound_apply(void);

static expr adjoin_arg(expr val, expr argl);

#pragma mark Initialization

int lisper_init()
{
  int status;
  expr vars, vals;

  status = mem_init();

  vars = listn(17,
               mk_sym("+"),
               mk_sym("*"),
               mk_sym("-"),
               mk_sym("eq?"),
               mk_sym(">"),
               mk_sym("<"),
               mk_sym("null?"),
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
  vals = listn(17,
               mk_prim(op_add),
               mk_prim(op_mul),
               mk_prim(op_sub),
               mk_prim(op_eq),
               mk_prim(op_gt),
               mk_prim(op_lt),
               mk_prim(op_is_null),
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
  __env = env_extend(vars, vals, NIL);

  return status;
}

void add_primitives(expr names, expr funcs)
{
  __env = env_extend(names, funcs, __env);
}

expr *lookup_name(const char *name)
{
  return env_lookup(mk_sym(name), __env);
}

#pragma mark Evaluator

expr eval(expr exp)
{
  expr val;
  long jmp;
  push_registers();
  jmp = mem_get_stackptr();

  __exp = exp;
  __continue = make_cont(NULL);
  __goto = eval_dispatch;

//  push(__env);
  while (__goto != NULL)
  {
    __goto();
  }
//  pop(__env);
  val = __val;
  mem_set_stackptr(jmp);
  pop_registers();
  return val;
}

static void eval_dispatch()
{

  if (is_self_eval(__exp))
  {
    ev_self();
  }
  else if (is_variable(__exp))
  {
    ev_variable();
  }
  else if (is_if(__exp))
  {
    ev_if();
  }
  else if (is_quote(__exp))
  {
    ev_quote();
  }
  else if (is_lambda(__exp))
  {
    ev_lambda();
  }
  else if (is_definition(__exp))
  {
    ev_definition();
  }
  else if (is_begin(__exp))
  {
    ev_begin();
  }
  // Check appl as last condition
  else if (is_application(__exp))
  {
    ev_application();
  }
  else
  {
    error("Unknown expression %d", __exp.type);
    exit(1);
  }
}

static void ev_self()
{
  __val = __exp;
  __goto = (cont_f)__continue.value;
}

static void ev_quote()
{
  __val = quote_text(__exp);
  __goto = (cont_f)__continue.value;
}

static void ev_variable()
{
  expr *p;
  p = env_lookup(__exp, __env);
  if (p == NULL)
  {
    error("Unbound variable %s", __exp.str);
    __val = NIL;
    __goto = NULL;
  }
  else
  {
    __val = *p;
    __goto = (cont_f)__continue.value;
  }
}

static void ev_lambda()
{
  __unev = lambda_params(__exp);
  __exp = lambda_body(__exp);
  __val = make_procedure(__unev, __exp, __env);
  __goto = (cont_f)__continue.value;
}

static void ev_application()
{
  push(__continue);
  push(__env);
  __unev = appl_operands(__exp);
  push(__unev);
  __exp = appl_operator(__exp);
  __continue = make_cont(ev_appl_did_operator);
  __goto = eval_dispatch;
}

static void ev_appl_did_operator()
{
  pop(__unev);
  pop(__env);
  __argl = NIL;
  __proc = __val;
  if (is_nil(__unev))
  {
    __goto = apply_dispatch;
  }
  else
  {
    __goto = ev_appl_operand_loop;
    push(__proc);
  }
}

static void ev_appl_operand_loop()
{
  push(__argl);
  __exp = operands_first(__unev);
  if (operands_is_last(__unev))
  {
    __goto = ev_appl_last_arg;
  }
  else
  {
    push(__env);
    push(__unev);
    __continue = make_cont(ev_appl_accum_arg);
    __goto = eval_dispatch;
  }
}
static void ev_appl_accum_arg()
{
  pop(__unev);
  pop(__env);
  pop(__argl);
  __argl = adjoin_arg(__val, __argl);
  __unev = operands_rest(__unev);
  __goto = ev_appl_operand_loop;
}

static void ev_appl_last_arg()
{
  __continue = make_cont(ev_appl_accum_last_arg);
  __goto = eval_dispatch;
}

static void ev_appl_accum_last_arg()
{
  pop(__argl);
  __argl = adjoin_arg(__val, __argl);
  pop(__proc);
  __goto = apply_dispatch;
}

static void ev_begin()
{
  __unev = begin_actions(__exp);
  push(__continue);
  __goto = ev_sequence;
}

#define TAIL_RECURSIVE 1

#if TAIL_RECURSIVE

static void ev_sequence()
{
  __exp = car(__unev); // Next expression
    if (is_nil(cdr(__unev))) // If last expression
  {
    __goto = ev_sequence_end;
  }
  else
  {
    push(__unev);
    push(__env);
    __continue = make_cont(ev_sequence_cont);
    __goto = eval_dispatch;
  }
}

static void ev_sequence_end()
{
  pop(__continue);
  __goto = eval_dispatch;
}

#else

static void ev_sequence()
{
  if (is_nil(__unev)) // If last expression
  {
    __goto = ev_sequence_end;
  }
  else
  {
    __exp = car(__unev); // Next expression
    push(__unev);
    push(__env);
    __continue = make_cont(ev_sequence_cont);
    __goto = eval_dispatch;
  }
}

static void ev_sequence_end()
{
  pop(__continue);
  __goto = (cont_f)__continue.value;
}

#endif

static void ev_sequence_cont()
{
  pop(__env);
  pop(__unev);
  __unev = cdr(__unev); // Rest expression
  __goto = ev_sequence;
}


static void apply_dispatch()
{
  if (is_prim(__proc))
  {
    __goto = primitive_apply;
  }
  else if (is_procedure(__proc))
  {
    __goto = compound_apply;
  }
  else
  {
    error("Unknown procedure");
    __goto = NULL;
  }
}

static void primitive_apply()
{
  __val = ((primitive_f)__proc.value)(__argl);
  pop(__continue);
  __goto = (cont_f)__continue.value;
}

static void compound_apply()
{
  __unev = procedure_params(__proc);
  __env = procedure_env(__proc);
  __env = env_extend(__unev, __argl, __env);
  __unev = procedure_body(__proc);
  __goto = ev_sequence;
}

static void ev_definition()
{
  __unev = definition_variable(__exp);
  push(__unev);
  __exp = definition_value(__exp);
  push(__env);
  push(__continue);
  __continue = make_cont(ev_definition_cont);
  __goto = eval_dispatch;
}

static void ev_definition_cont()
{
  pop(__continue);
  pop(__env);
  pop(__unev);
  env_define_variable(__unev, __val, __env);
  __val = mk_num(1);
  __goto = (cont_f)__continue.value;
}

static void ev_if()
{
  push(__exp);
  push(__env);
  push(__continue);
  __continue = make_cont(ev_if_decide);
  __exp = if_predicate(__exp);
  __goto = eval_dispatch;
}

static void ev_if_decide()
{
  pop(__continue);
  pop(__env);
  pop(__exp);

  if (__val.intv != 0)
    __goto = ev_if_consequent;
  else
    __goto = ev_if_alternative;
}

static void ev_if_alternative()
{
  __exp = if_alternative(__exp);
  __goto = eval_dispatch;
}

static void ev_if_consequent()
{
  __exp = if_consequent(__exp);
  __goto = eval_dispatch;
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

static expr adjoin_arg(expr val, expr argl)
{
  return list_append(argl, val);
}
