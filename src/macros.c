#include "macros.h"
#include "match.h"
#include "expand.h"
#include "logger.h"
#include "printer.h"
#include "pattern.h"

static bool macros_add_syntax_transformer(struct macros_expander *expander, expr exp);
static expr macros_transform(struct machine *m, expr transformer, expr exp);

struct macros_expander *macros_create(struct machine *m)
{
  struct macros_expander *expander = malloc(sizeof(struct macros_expander));
  struct array *a = memory_alloc_root(m->memory, 1);
  expander->machine = m;
  expander->macros = a->cells;
  array_push(a, NIL); // Initially no macros

  return expander;
}

void macros_destroy(struct macros_expander *expander)
{
  free(expander);
}

expr macros_collect(struct macros_expander *expander, expr exp)
{
  expr current, source;

  if (!is_pair(exp))
    return exp; // Nothing to collect

  source = NIL;
  current = exp;
  while (!is_nil(current))
  {
    if (is_define_syntax(car(current)))
    {
      bool success = macros_add_syntax_transformer(expander, car(current));
      if (!success)
        return FALSE;
    }
    else
    {
      source = cons(expander->machine, car(current), source);
    }
    current = cdr(current);
  }
  return list_reverse(expander->machine, source);
}

expr macros_expand(struct macros_expander *expander, expr exp)
{
  struct machine *m = expander->machine;

  if (is_pair(exp))
  {
    expr head = car(exp);
    if (is_sym(head))
    {
      expr ptransformer = assoq(head, expander->macros[0]);
      if (!is_false(ptransformer))
      {
        return macros_expand(expander, macros_transform(m, cdr(ptransformer), exp));
      }
    }
    // Otherwise recursively expand car/cdr
    expr head_expanded = macros_expand(expander, head);
    return cons(m, head_expanded, macros_expand(expander, cdr(exp)));
  }
  return exp; // number, symbol, nil
}

static expr macros_transform(struct machine *m, expr transformer, expr exp)
{
  expr patterns = syntax_transformer_patterns(transformer);
  logexpr("All Patterns", patterns);
  expr literals = syntax_transformer_literals(transformer);
  while (!is_nil(patterns))
  {
    expr pattern = syntax_pattern(car(patterns));
    expr template = syntax_template(car(patterns));

    expr bindings = match_pattern(m, pattern, exp, literals);
    logexpr("Transforming", exp);
    logexpr("With pattern", pattern);
    logexpr("Got bindings", bindings);
    if (!is_false(bindings))
    {
      expr patternvars = pattern_depths(m, pattern, literals);
      return expand_template(m, template, 0, bindings, patternvars, NIL);
    }
    logexpr("Patterns", patterns);
    patterns = cdr(patterns);
    logexpr("Patterns after", patterns);
  }
  error("No pattern matched for");
  print_exp(exp);
  printf("\n");
  return FALSE;
}

static bool macros_add_syntax_transformer(struct macros_expander *expander, expr exp)
{
  struct machine *m = expander->machine;
  logexpr("Defining syntax", exp);
  expr name = define_syntax_name(exp);
  expr rules = define_syntax_rules(exp);

  if (!is_syntax_rules(rules))
  {
    error("Expected syntax-rules in define-syntax, got: %d", rules.type);
    return false;
  }

  // Extract literals and patterns
  expr literals = syntax_rules_literals(rules);
  expr patterns = syntax_rules_patterns(rules);

  // Create the syntax transformer
  expr transformer = make_syntax_transformer(m, literals, patterns);
  logexpr("Created syntax transformer", transformer);
  logexpr("For name", name);
  expander->macros[0] = cons(m, cons(m, name, transformer), expander->macros[0]);

  return true;
}

expr macros_preprocess(struct macros_expander *expander, expr source)
{
  struct machine *m = expander->machine;
  memory_enable_gc(m->memory, false);
  logexpr("Source", source);
  source = macros_collect(expander, source);
  if (is_false(source))
    return FALSE;
  logexpr("Source (preprocessed)", source);
  source = macros_expand(expander, source);
  logexpr("Source (expanded)", source);
  machine_push(m, source);
  memory_enable_gc(m->memory, true);
  machine_pop(m, &source);
  return source;
}