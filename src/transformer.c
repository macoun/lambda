#include "transformer.h"
#include "match.h"
#include "expand.h"
#include "pattern.h"
#include "logger.h"

expr syntax_transformer_match(struct machine *m, expr transformer, expr exp)
{
  expr rules = syntax_transformer_rules(transformer);
  expr literals = syntax_transformer_literals(transformer);

  if (is_nil(rules))
  {
    error("No rules in syntax transformer");
    return FALSE;
  }

  while (!is_nil(rules))
  {
    expr rule = car(rules);
    expr pattern = syntax_pattern(rule);
    expr bindings = match_pattern(m, pattern, exp, literals);
    if (!is_false(bindings))
      return cons(m, rule, bindings);
    rules = cdr(rules);
  }
  return FALSE;
}

expr syntax_transformer_apply(struct machine *m, expr transformer, expr exp)
{
  // logexpr("Applying transformer", car(transformer));

  expr match = syntax_transformer_match(m, transformer, exp);
  if (is_false(match))
    return FALSE;

  expr rule = car(match);
  expr bindings = cdr(match);
  expr pattern = syntax_pattern(rule);
  expr template = syntax_template(rule);
  expr literals = syntax_transformer_literals(transformer);

  expr patternvars = pattern_depths(m, pattern, literals);
  expr expanded = expand_template(m, template, 0, bindings, patternvars, NIL);
  return expanded;
}
