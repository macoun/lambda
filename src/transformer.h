#ifndef transformer_h
#define transformer_h

#include "exp.h"

// Syntax transformers
#define is_syntax_transformer(e) is_tagged((e), "syntax-transformer")
#define make_syntax_transformer(m, literals, rules) \
  listn(m, 3, mk_sym("syntax-transformer"), literals, rules)
#define syntax_transformer_literals(e) cadr(e)
#define syntax_transformer_rules(e) caddr(e)

#define is_syntax_pattern(e) is_pair(e) && is_pair(cdr(e))
#define make_syntax_pattern(m, pattern, template, pattern_vars) \
  listn(m, 3, pattern, template, pattern_vars)
#define syntax_pattern(e) car(e)
#define syntax_template(e) cadr(e)

expr syntax_transformer_apply(struct machine *m, expr transformer, expr exp);

#endif