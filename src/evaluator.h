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

struct evaluator
{
  struct machine *machine;
  struct memory *memory;
  void *goto_fn;
};

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

typedef void (*cont_f)(struct evaluator *);
typedef expr (*primitive_f)(struct machine *, expr);

struct evaluator *evaluator_create();
void evaluator_destroy(struct evaluator *ev);
expr eval(struct evaluator *ev, expr exp);

void add_primitives(struct machine *m, expr names, expr funcs);

#define make_cont(f) mk_cell(CUSTOM, f)

#define is_tagged(e, tag) \
  !is_nil(e) && is_pair(e) && is_sym(car(e)) && !strcasecmp(car(e).str, tag)

// Self-evaluating
#define is_self_eval(e) \
  (is_num(e) || is_str(e) || is_nil(e) || is_real_true(e) || is_false(e))

// Variable
#define is_variable(e) is_sym(e)

// Lambda
#define is_lambda(e) is_tagged(e, "lambda")
#define make_lambda(m, params, body) \
  cons(m, mk_sym("lambda"), cons(m, params, body))
#define lambda_params(e) cadr(e)
#define lambda_body(e) cddr(e)

// if
#define is_if(e) is_tagged(e, "if")
#define if_predicate(e) cadr(e)
#define if_consequent(e) caddr(e)
#define if_alternative(e) car(cdddr(e))

// Quote
#define is_quote(e) is_tagged(e, "quote")
#define make_quote(m, e) \
  cons(m, mk_sym("quote"), cons(m, e, NIL))
#define quote_text(e) cadr(e)

// Quasiquote
#define is_quasiquote(e) is_tagged(e, "quasiquote")
#define quasiquote_text(e) cadr(e)

// Unquote
#define is_unquote(e) is_tagged(e, "unquote")
#define unquote_text(e) cadr(e)

// Unquote splicing
#define is_unquote_splicing(e) is_tagged(e, "unquote-splicing")
#define unquote_splicing_text(e) cadr(e)

// Procedure
#define is_procedure(e) is_tagged(e, "procedure")
#define make_procedure(machine, params, body, env) \
  listn(machine, 4, mk_sym("procedure"), params, body, env)
#define procedure_params(p) cadr(p)
#define procedure_body(p) caddr(p)
#define procedure_env(p) car(cdddr(p))

// Macro definition
#define is_macro_definition(e) is_tagged(e, "define-macro")
#define is_macro(e) is_tagged(e, "macro")
#define make_macro(machine, pattern, template, env) \
  listn(machine, 4, mk_sym("macro"), pattern, template, env)
#define macro_pattern(p) cadr(p)
#define macro_template(p) caddr(p)
#define macro_env(p) car(cdddr(p))

// Define Syntax macros
#define is_define_syntax(e) is_tagged(e, "define-syntax")
#define define_syntax_name(e) cadr(e)
#define define_syntax_rules(e) caddr(e)

#define is_syntax_rules(e) is_tagged(e, "syntax-rules")
#define syntax_rules_literals(e) cadr(e)
#define syntax_rules_patterns(e) cddr(e)

#define is_syntax_pattern(e) is_pair(e) && is_pair(cdr(e))
#define syntax_pattern(e) car(e)
#define syntax_template(e) cadr(e)

// Syntax transformers
#define is_syntax_transformer(e) is_tagged((e), "syntax-transformer")
#define make_syntax_transformer(m, literals, patterns) \
  listn(m, 3, mk_sym("syntax-transformer"), literals, patterns)
#define syntax_transformer_literals(e) cadr(e)
#define syntax_transformer_patterns(e) caddr(e)

// Ellipsis
#define mk_ellipsis() mk_sym("...")
#define is_ellipsis(e) (is_sym(e) && !strcasecmp(e.str, "..."))
#define is_ellipsis_pattern(e) (is_pair(e) && is_ellipsis(cadr(e)))

// Underscore
#define mk_underscore() mk_sym("_")
#define is_underscore(e) (is_sym(e) && !strcasecmp(e.str, "_"))

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
  ((is_sym(cadr(e))) ? cadr(e) : caadr(e))

#define definition_value(m, e) ( \
    (is_sym(cadr(e))) ? caddr(e) : make_lambda(m, cdadr(e), cddr(e)))

// Evaluator developers only
void push_reg(struct evaluator *m, int reg);
void pop_reg(struct evaluator *m, int reg);
void mov_reg(struct evaluator *m, int srcreg, int destreg);
void set_reg(struct evaluator *m, int reg, struct cell val);
struct cell get_reg(struct evaluator *m, int reg);

#endif /* evaluator_h */
