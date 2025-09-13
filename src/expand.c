#include "expand.h"
#include "printer.h"
#include "logger.h"
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

expr lookup_var(expr var, expr bindings, expr patternvars, int depth, expr idxstack)
{
  expr binding = assoq(var, bindings);
  if (is_false(binding))
    return FALSE; // or handle unbound

  expr val = cdr(binding);
  expr pvar = assoq(var, patternvars);
  if (is_false(pvar))
    return val; // not a pattern var

  int pdepth = cdr(pvar).intv; // number of indices to consume
  if (pdepth == 0)
    return val;

  int stack_len = list_length(idxstack);
  if (stack_len < pdepth)
    pdepth = stack_len;

  expr cur = val;
  int start = stack_len - pdepth;
  int len = pdepth < depth ? pdepth : depth;
  for (int i = 0; i < len; i++)
  {
    expr idx = list_ref(idxstack, mk_num(start + i));
    if (!is_pair(cur))
    {
      error("lookup_var: expected list while indexing");
      print_exp(cur);
      printf("\n");
      return FALSE;
    }
    cur = list_ref(cur, idx);
  }
  return cur;
}

expr pattern_depths(struct machine *m, expr pattern, expr literals)
{
  expr pvars = pattern_vars(m, pattern, 0, literals, NIL);
  return pvars;
}

expr expand_template(struct machine *m, expr template, int depth, expr bindings, expr patternvars, expr idxstack)
{
  if (is_ellipsis(template))
  {
    return template; // Ellipsis should be handled in the parent context
  }
  else if (is_sym(template))
  {
    expr binding = assoq(template, bindings);
    if (!is_false(binding))
    {
      return lookup_var(template, bindings, patternvars, depth, idxstack);
    }
    else
    {
      return template; // Not a pattern variable, leave unchanged
    }
  }

  if (!is_pair(template))
  {
    return template; // Not a pair, leave unchanged
  }

  if (is_ellipsis_pattern(template))
  {
    expr subtemplate = car(template);
    expr rest = cddr(template); // skip subtemplate and ellipsis
    expr sbtvars = pattern_vars(m, subtemplate, 0, NIL, NIL);

    /* Choose controlling variables:
       Pick the variable(s) with the maximum pattern-depth that appear
       in the subtemplate. Using the deepest variable(s) ensures nested
       ellipses expand in the correct order. */
    expr ctlbindings = NIL;

    /* Find maximum depth among variables in subtemplate */
    int max_depth = -1;
    expr scan = sbtvars;
    while (!is_nil(scan))
    {
      expr v = car(scan);
      expr pvar = assoq(car(v), patternvars);
      if (!is_false(pvar))
      {
        int d = cdr(pvar).intv;
        if (d > max_depth)
          max_depth = d;
      }
      scan = cdr(scan);
    }

    /* Collect variables whose depth == max_depth */
    scan = sbtvars;
    while (!is_nil(scan))
    {
      expr sbtvar = car(scan);
      expr varname = car(sbtvar);
      expr pvar = assoq(varname, patternvars);
      int pdepth = cdr(pvar).intv;
      if (!is_false(pvar) && pdepth == max_depth)
      {
        expr pval = assoq(car(pvar), bindings);
        if (is_false(pval))
        {
          error("No binding found for pattern variable in template");
          return FALSE; // No binding found, invalid template
        }
        expr cval = lookup_var(varname, bindings, patternvars, pdepth - 1, idxstack);
        expr ctlbind = cons(m, varname, cval);
        ctlbindings = cons(m, ctlbind, ctlbindings);
      }
      scan = cdr(scan);
    }

    if (is_nil(ctlbindings))
    {
      // No controlling variable found for ellipsis in template. One iteration
      expr newidxstack = list_append(m, idxstack, cons(m, mk_num(0), NIL));
      return expand_template(m, subtemplate, depth + 1, bindings, patternvars, newidxstack);
    }

    /* Get the first binding's values to determine length */
    expr firstlst = cdr(car(ctlbindings));
    int len = list_length(firstlst);

    /* Verify all controlling bindings have the same length */
    expr cur = cdr(ctlbindings);
    while (!is_nil(cur))
    {
      expr val = cdr(car(cur));
      if (list_length(val) != len)
      {
        error("Mismatched lengths in ellipsis expansion");
        return FALSE;
      }
      cur = cdr(cur);
    }

    /* Create environment for each expansion */
    expr result = NIL;
    for (int i = 0; i < len; i++)
    {
      expr newidxstack = list_append(m, idxstack, cons(m, mk_num(i), NIL));
      expr subres = expand_template(m, subtemplate, depth + 1, bindings, patternvars, newidxstack);
      result = cons(m, subres, result);
    }
    result = list_reverse(m, result);

    if (!is_nil(rest))
    {
      expr restres = expand_template(m, rest, depth, bindings, patternvars, idxstack);
      result = list_append(m, result, restres);
    }
    return result;
  }

  // Regular pair, expand car and cdr
  expr var = car(template);
  expr expanded_var = expand_template(m, var, depth, bindings, patternvars, idxstack);
  expr rest = cdr(template);
  expr expanded_rest = expand_template(m, rest, depth, bindings, patternvars, idxstack);
  return cons(m, expanded_var, expanded_rest);
}
