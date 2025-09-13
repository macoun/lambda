#include "match.h"
#include "printer.h"
#include "logger.h"
#include "evaluator.h"
#include "pattern.h"

expr match_list_p(struct machine *m, expr *pattern, expr *input, expr literals)
{
  expr bindings = NIL;
  expr pat = *pattern;
  expr inp = *input;
  while (is_pair(pat) && is_pair(inp) && !is_ellipsis_pattern(pat))
  {
    expr var = car(pat);
    expr val = car(inp);

    expr matched = match_pattern(m, var, val, literals);

    if (is_false(matched))
      return FALSE; // No match in car
    while (!is_nil(matched))
    {
      bindings = cons(m, car(matched), bindings);
      matched = cdr(matched);
    }
    pat = cdr(pat);
    inp = cdr(inp);
  }

  *pattern = pat;
  *input = inp;

  return bindings;
}

expr match_list_pattern(struct machine *m, expr pattern, expr input, expr literals)
{

  if (!is_pair(input))
    return FALSE; // No match

  // Match element by element
  expr bindings = match_list_p(m, &pattern, &input, literals);

  if (!is_ellipsis_pattern(pattern))
  {
    if (!is_nil(pattern) || !is_nil(input))
      return FALSE; // Length mismatch
    return bindings;
  }

  // Ellipsis found, split pattern into three parts: before ellipsis, ellipsis part, after ellipsis

  expr ellipsis_part = car(pattern);  // variable before ...
  expr after_pattern = cddr(pattern); // skip var and ellipsis

  int rest_value_count = list_length(input);
  int rest_pattern_count = list_length(after_pattern);

  if (rest_value_count < rest_pattern_count)
    return FALSE; // Not enough values to match the rest patterns

  expr sbtvars = NIL;
  expr bind = pattern_depths(m, ellipsis_part, literals);
  while (!is_nil(bind))
  {
    sbtvars = cons(m, caar(bind), sbtvars);
    bind = cdr(bind);
  }

  expr ellipsis_bindings = NIL;
  for (int i = 0; i < rest_value_count - rest_pattern_count && !is_nil(input); i++)
  {
    expr val = car(input);
    expr eb = match_pattern(m, ellipsis_part, val, literals);

    if (is_false(eb))
      return FALSE; // No match in ellipsis part

    expr keys = sbtvars;
    while (!is_nil(keys))
    {
      expr key = car(keys);
      expr ebind = assoq(key, eb);
      bind = assoq(key, ellipsis_bindings);
      if (is_false(bind))
      {
        expr initial = cons(m, key, cons(m, cdr(ebind), NIL));
        ellipsis_bindings = cons(m, initial, ellipsis_bindings);
      }
      else
      {
        list_append_x(m, cdr(bind), cons(m, cdr(ebind), NIL));
      }
      keys = cdr(keys);
    }
    if (is_pair(input))
      input = cdr(input);
  }

  if (is_nil(ellipsis_bindings))
  {
    // No matches in ellipsis part, ensure all pattern variables are bound to empty lists
    expr keys = sbtvars;
    while (!is_nil(keys))
    {
      expr key = car(keys);
      expr bind = assoq(key, ellipsis_bindings);
      if (is_false(bind))
      {
        expr initial = cons(m, key, NIL);
        ellipsis_bindings = cons(m, initial, ellipsis_bindings);
      }
      keys = cdr(keys);
    }
  }

  // Now handle rest patterns
  expr after_ellipsis = match_list_p(m, &after_pattern, &input, literals);
  bindings = list_append_x(m, bindings, ellipsis_bindings);
  expr result = list_append_x(m, bindings, after_ellipsis);
  return result;
}

expr match_pattern(struct machine *m, expr pattern, expr input, expr literals)
{
  if (is_underscore(pattern))
  {
    return NIL; // Wildcard matches anything
  }
  else if (is_sym(pattern))
  {
    expr lit = memq(pattern, literals);
    if (!is_false(lit))
    {
      if (is_eq(pattern, car(lit)))
      {
        return NIL; // Literal matches itself
      }
      else
        return FALSE; // No match
    }
    // Variable pattern, bind variable to expression
    return cons(m, cons(m, pattern, input), NIL);
  }
  else if (is_pair(pattern))
  {
    return match_list_pattern(m, pattern, input, literals);
  }
  else if (is_equal(pattern, input))
  {
    return NIL; // Both are equal atoms
  }
  return FALSE; // No match
}
