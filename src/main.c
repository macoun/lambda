//
//  main.c
//  Lisper
//
//  Created by Ferhat Ayaz on 09/03/2016.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include "evaluator.h"
#include "reader.h"
#include "printer.h"
#include "logger.h"
#include "machine.h"
#include "repl.h"
#include "macros.h"

char *read_file(const char *fname)
{
  char *buffer;
  long size;
  FILE *f;

  f = fopen(fname, "r");
  if (f == NULL)
  {
    fprintf(stderr, "Could not open %s\n", fname);
    return NULL;
  }

  fseek(f, 0L, SEEK_END);
  size = ftell(f);
  rewind(f);

  buffer = calloc(size + 1, sizeof(char));
  fread(buffer, 1, size, f);
  *(buffer + size) = '\0';

  return buffer;
}

expr read_exp_from_file(struct machine *m, const char *fname)
{
  char *buffer, *p;
  expr exp, source;
  int err;

  source = NIL;
  buffer = read_file(fname);
  p = buffer;
  for (; *p != '\0';)
  {
    exp = parse_exp(m, &p, &err);
    if (err == PERR_FIN)
      break;

    if (err != 0)
    {
      error("Error: %d\n", err);
      source = FALSE;
      break;
    }

    machine_push(m, exp);

    source = cons(m, exp, source);

    machine_pop(m, &exp);
  }

  free(buffer);
  if (!is_false(source))
    source = list_reverse(m, source);
  return source;
}

int evaluate_file(struct evaluator *ev, const char *fname)
{
  struct machine *m = ev->machine;
  expr source = read_exp_from_file(m, fname);
  if (is_false(source))
    return -1;
  logexpr("Source", source);
  memory_enable_gc(m->memory, false);
  struct macros_expander *expander = macros_create(m);
  source = macros_collect(expander, source);
  logexpr("Source (preprocessed)", source);
  source = macros_expand(expander, source);
  logexpr("Source (expanded)", source);
  machine_push(m, source);
  memory_enable_gc(m->memory, true);
  machine_pop(m, &source);
  expr current = source;
  while (!is_nil(current))
  {
    expr exp = car(current);
    machine_push(m, current);
    eval(ev, exp);
    machine_pop(m, &current);
    current = cdr(current);
  }
  macros_destroy(expander);
  return 0;
}

expr prim_say_hello(expr args)
{
  expr name;

  name = car(args);

  if (is_str(name))
  {
    printf("Hello %s\n", name.str);
  }
  else
  {
    printf("What?\n");
  }
  return mk_num(1);
}

int main(int argc, const char *argv[])
{
  struct evaluator *ev;

  ev = evaluator_create();
  if (!ev)
  {
    error("Cannot initialize Lisper");
    exit(1);
  }
  info("Lisper initialized");
  add_primitives(ev->machine, list(ev->machine, mk_sym("say-hello"), NIL),
                 list(ev->machine, mk_prim(prim_say_hello), NIL));

  if (argc > 1)
  {
    evaluate_file(ev, argv[1]);
  }
  else
    repl(ev, argc, argv);

  // Print detailed stats
  memory_print_stats(ev->memory);

  evaluator_destroy(ev);
  return 0;
}
