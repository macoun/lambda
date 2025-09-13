#include "pattern.h"
#include "evaluator.h"

expr pattern_vars(struct machine *m, expr pattern, int depth, expr literals, expr bindings)
{
  if (is_underscore(pattern) || is_ellipsis(pattern))
  {
    return bindings; // Wildcard, do nothing
  }
  else if (is_sym(pattern))
  {
    expr lit = memq(pattern, literals);
    if (!is_false(lit))
    {
      return bindings; // Literal, do nothing
    }
    // Variable pattern, bind variable to expression
    expr existing = assoq(pattern, bindings);
    if (is_false(existing))
    {
      expr bind = cons(m, pattern, mk_num(depth));
      return cons(m, bind, bindings);
    }
    else
    {
      return bindings; // Already bound, do nothing
    }
  }
  else if (is_pair(pattern))
  {
    int len = list_length(pattern);
    int i = 0;
    expr cur = pattern;
    while (i < len)
    {
      expr var = car(cur);
      if (is_ellipsis_pattern(cur))
      {
        // Ellipsis pattern, increase depth
        bindings = pattern_vars(m, var, depth + 1, literals, bindings);
        cur = cdr(cdr(cur)); // Skip var and ellipsis
        i += 2;
      }
      else
      {
        bindings = pattern_vars(m, var, depth, literals, bindings);
        cur = cdr(cur);
        i++;
      }
    }
    return bindings;
  }
  return bindings; // number, string, nil, leave unchanged
}

expr pattern_depths(struct machine *m, expr pattern, expr literals)
{
  expr pvars = pattern_vars(m, pattern, 0, literals, NIL);
  return pvars;
}
