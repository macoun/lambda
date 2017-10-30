//
//  evaluator.h
//  Lisper
//
//  Created by Ferhat Ayaz on 09/03/2016.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#ifndef evaluator_h
#define evaluator_h

#include "exp.h"

#include <string.h>

typedef expr (*primitive_f)(expr);
extern expr *__registers[];

int lisper_init(void);
void add_primitives(expr names, expr funcs);
expr *lookup_name(const char *name);

void push_registers(void);
void pop_registers(void);

#define is_tagged(e, tag) \
  !is_nil(e) && is_pair(e) && is_sym(car(e)) \
  && !strcasecmp(car(e).str, tag)


// Self-evaluating
#define is_self_eval(e) \
  (is_num(e) || is_str(e) || is_nil(e))

// Variable
#define is_variable(e) is_sym(e)

// Lambda
#define is_lambda(e) is_tagged(e, "lambda")
#define make_lambda(params, body) \
  cons(mk_sym("lambda"), cons(params, body))
#define lambda_params(e) cadr(e)
#define lambda_body(e) cddr(e)

// if
#define is_if(e) is_tagged(e, "if")
#define if_predicate(e) cadr(e)
#define if_consequent(e) caddr(e)
#define if_alternative(e) car(cdddr(e))

// Quote
#define is_quote(e) is_tagged(e, "quote")
#define make_quote(e) \
  cons(mk_sym("quote"), cons(e, NIL))
#define quote_text(e) cadr(e)

// Procedure
#define is_procedure(e) is_tagged(e, "procedure")
#define make_procedure(params, body, env) \
  listn(4, mk_sym("procedure"), params, body, env)
#define procedure_params(p) cadr(p)
#define procedure_body(p) caddr(p)
#define procedure_env(p) car(cdddr(p))

// Application
#define is_application(e) is_pair(e)
#define appl_operator(e) car(e)
#define appl_operands(e) cdr(e)
#define operands_is_last(e) is_nil(cdr(e))
#define operands_first(e) car(e)
#define operands_rest(e) cdr(e)

// Sequence
#define is_begin(e) is_tagged(e, "begin")
#define begin_actions(e) cdr(e)

// Definition
#define is_definition(e) \
  is_tagged(e, "define") || is_tagged(e, "def")

#define definition_variable(e) \
  ((is_sym(cadr(e)))?cadr(e):caadr(e))

#define definition_value(e) (\
  (is_sym(cadr(e)))? \
    caddr(e) : \
    make_lambda(cdadr(e), cddr(e)))

#endif /* evaluator_h */
