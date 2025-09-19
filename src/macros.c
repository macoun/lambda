#include "macros.h"
#include "match.h"
#include "expand.h"
#include "logger.h"
#include "printer.h"
#include "pattern.h"
#include "env.h"
#include "transformer.h"
#include <assert.h>

static expr hygienic_rename(struct machine *m, expr patternvars, expr exp, expr scope, expr env);
static expr hygienic_bind(struct machine *m, expr patternvars, expr template, expr env);
static expr hygienic_gensym(struct machine *m, expr scopebinding);
static expr hygienic_generate_scope_id();

expr make_scoped_symbol(struct machine *m, expr sym, expr binding)
{
  if (!is_sym(sym))
  {
    error("Expected symbol to make scoped symbol, got type %d", sym.type);
    return FALSE;
  }
  expr pair = cons(m, sym, binding);
  expr scoped = mk_cell(STRUCT, SCOPED_SYMBOL, {.array = pair.array});
  return scoped;
}

struct macros_expander *macros_create(struct machine *m)
{
  struct macros_expander *expander = malloc(sizeof(struct macros_expander));
  struct array *a = memory_alloc_root(m->memory, 1);
  array_push(a, NIL);

  expander->machine = m;
  expander->macros = a->cells;
  expander->macros[0] = env_empty(m);

  return expander;
}

void macros_destroy(struct macros_expander *expander)
{
  free(expander);
}

expr macros_collect(struct macros_expander *expander, expr exp, expr env)
{
  struct machine *m = expander->machine;
  expr current, source;

  if (!is_pair(exp))
    return exp; // Nothing to collect

  source = NIL;
  current = exp;
  while (!is_nil(current))
  {
    expr defstx = car(current);
    if (is_define_syntax(defstx))
    {
      expr name = define_syntax_name(defstx);
      expr rules = define_syntax_rules(defstx);

      if (!is_syntax_rules(rules))
      {
        error("Expected syntax-rules in define-syntax, got: %d", rules.type);
        return FALSE;
      }

      expr literals = syntax_rules_literals(rules);
      expr patterns = syntax_rules_patterns(rules);
      expr hygienic_patterns = NIL;
      expr head = patterns;
      while (!is_nil(head))
      {
        expr pattern = syntax_pattern(car(head));
        expr template = syntax_template(car(head));
        expr patternvars = pattern_vars(m, pattern, literals);
        expr hygienic_template = hygienic_bind(m, patternvars, template, env);
        expr stxpattern = make_syntax_pattern(m, pattern, hygienic_template, patternvars);
        hygienic_patterns = cons(m, stxpattern, hygienic_patterns);
        head = cdr(head);
      }
      hygienic_patterns = list_reverse(m, hygienic_patterns);
      expr transformer = make_syntax_transformer(m, literals, hygienic_patterns);
      env_define_variable(m, name, transformer, expander->macros[0]);
    }
    else
    {
      source = cons(expander->machine, car(current), source);
    }
    current = cdr(current);
  }
  return list_reverse(expander->machine, source);
}

expr macros_expand(struct macros_expander *expander, expr exp, expr env)
{
  struct machine *m = expander->machine;
  if (is_nil(expander->macros[0]))
    error("No environment for macros");

  if (is_pair(exp))
  {
    expr head = car(exp);
    expr ptransformer = FALSE;

    if (is_sym(head))
    {
      expr binding = env_lookup(head, expander->macros[0]);

      if (!is_false(binding))
      {
        ptransformer = cdr(binding);
      }
    }
    else if (is_scoped_sym(head))
    {
      ptransformer = cdr(head.array[1]);
      if (!is_syntax_transformer(ptransformer))
      {
        // logexpr("Not a transformer", head.array[0]);
        ptransformer = FALSE;
      }
    }

    if (!is_false(ptransformer))
    {
      expr transformed = syntax_transformer_apply(m, ptransformer, exp);
      expr keepenv = expander->macros[0];
      expander->macros[0] = env_extend_with_frame(m, NIL, keepenv);
      expr preprocessed = macros_collect(expander, transformed, env);
      expr renamed = hygienic_rename(m, NIL, preprocessed, env_empty(m), env);
      expr expanded = macros_expand(expander, renamed, env);
      expander->macros[0] = keepenv;
      return expanded;
    }

    // Otherwise recursively expand car/cdr
    expr head_expanded = macros_expand(expander, head, env);
    return cons(m, head_expanded, macros_expand(expander, cdr(exp), env));
  }
  return exp; // number, symbol, nil
}

expr macros_preprocess(struct macros_expander *expander, expr source, expr env)
{
  struct machine *m = expander->machine;
  memory_enable_gc(m->memory, false);
  logexpr("Source", source);
  source = macros_collect(expander, source, env);
  if (is_false(source))
    return FALSE;
  logexpr("Source (preprocessed)", source);
  source = macros_expand(expander, source, env_extend_with_frame(m, NIL, env));
  logexpr("Source (expanded)", source);
  machine_push(m, source);
  memory_enable_gc(m->memory, true);
  machine_pop(m, &source);
  return source;
}

static expr hygienic_gensym(struct machine *m, expr scopebinding)
{
  expr sym = car(scopebinding);
  expr scope_id = cdr(scopebinding);
  int need = snprintf(NULL, 0, "%s#%d", sym.str, scope_id.intv) + 1;
  char *buf = malloc((size_t)need);
  snprintf(buf, (size_t)need, "%s#%d", sym.str, scope_id.intv);

  expr result = mk_sym(buf);
  free(buf);
  return result;
}

static expr hygienic_bind(struct machine *m, expr patternvars, expr template, expr env)
{
  if (is_sym(template))
  {
    if (!is_false(memq(template, patternvars)))
    {
      return template;
    }

    expr binding = env_lookup(template, env);
    if (!is_false(binding))
    {
      return make_scoped_symbol(m, template, binding);
    }
  }
  else if (is_pair(template))
  {
    expr new_head = hygienic_bind(m, patternvars, car(template), env);
    expr new_tail = hygienic_bind(m, patternvars, cdr(template), env);
    return cons(m, new_head, new_tail);
  }
  return template;
}

static expr hygienic_generate_scope_id()
{
  static int counter = -1;
  counter++;
  return mk_num(counter);
}

static expr hygienic_rename(struct machine *m, expr patternvars, expr exp, expr scope, expr env)
{

  if (is_sym(exp))
  {
    expr scopebinding = env_lookup(exp, scope);
    if (!is_false(scopebinding))
    {
      return hygienic_gensym(m, scopebinding);
    }
    return exp;
  }
  else if (is_lambda(exp))
  {
    expr params = lambda_params(exp);
    expr body = lambda_body(exp);

    expr new_scope_id = hygienic_generate_scope_id();
    expr new_scope = env_extend_with_frame(m, NIL, scope);
    expr new_params = NIL;
    expr head = params;

    while (!is_nil(head))
    {
      expr var = car(head);
      if (is_false(env_lookup(var, new_scope)))
      {
        env_define_variable(m, var, new_scope_id, new_scope);
      }
      var = hygienic_rename(m, patternvars, var, new_scope, env);
      new_params = cons(m, var, new_params);
      head = cdr(head);
    }

    expr new_body = hygienic_rename(m, patternvars, body, new_scope, env);
    expr lambda = cons(m, mk_sym("lambda"), cons(m, list_reverse(m, new_params), new_body));
    return lambda;
  }
  else if (is_definition(exp))
  {
    expr op = car(exp);
    expr var = cadr(exp);
    expr val = caddr(exp);

    expr new_op = hygienic_rename(m, patternvars, op, scope, env);
    expr new_var = hygienic_rename(m, patternvars, var, scope, env);
    expr new_val = hygienic_rename(m, patternvars, val, scope, env);
    return cons(m, new_op, cons(m, new_var, cons(m, new_val, NIL)));
  }
  else if (is_pair(exp))
  {
    expr head = hygienic_rename(m, patternvars, car(exp), scope, env);
    expr rest = hygienic_rename(m, patternvars, cdr(exp), scope, env);
    return cons(m, head, rest);
  }
  else
  {
    return exp;
  }
}
