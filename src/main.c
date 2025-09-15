//
//  main.c
//  Lisper
//
//  Created by Ferhat Ayaz on 09/03/2016.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include <libgen.h>
#include <sys/stat.h>
#include "evaluator.h"
#include "reader.h"
#include "printer.h"
#include "logger.h"
#include "machine.h"
#include "repl.h"
#include "macros.h"
#include "primitives.h"
#include "env.h"

void evaluate_expressions(struct evaluator *ev, expr source)
{
  struct machine *m = ev->machine;
  expr lambda = cons(m, mk_sym("lambda"), cons(m, NIL, source));
  expr appl = cons(m, lambda, NIL);
  eval(ev, appl);
}

int evaluate_file(struct evaluator *ev, struct macros_expander *expander, const char *fname)
{
  int err = 0;
  expr source = parse_from_file(ev->machine, fname, &err);
  if (err != PERR_FIN && err != PERR_OK)
  {
    error("Error parsing file %s: %d", fname, err);
    return err;
  }
  source = macros_preprocess(expander, source);
  if (is_false(source))
    return err;
  evaluate_expressions(ev, source);
  return err;
}

void load_modules(struct evaluator *ev, struct macros_expander *expander, const char *modules_dir)
{
  // expr env = primitives_env(ev->machine);
  char fname[1024];
  snprintf(fname, sizeof(fname), "%s/base.scm", modules_dir);
  info("Loading base module from %s", fname);
  evaluate_file(ev, expander, fname);
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

bool find_modules_dir(const char *program_path, char *dest)
{
  char buffer[1024], *dir;
  struct stat sb;
  strncpy(buffer, program_path, sizeof(buffer));
  dir = dirname(buffer);
  if (dir == NULL)
    return false; // iterate through PATH
  snprintf(dest, sizeof(buffer), "%s/../modules", dir);

  return (stat(dir, &sb) == 0 && S_ISDIR(sb.st_mode));
}

void add_primitives(struct machine *m, expr names, expr funcs)
{
  machine_set_reg(m, ENV, env_extend(m, names, funcs, machine_get_reg(m, ENV)));
}

int main(int argc, const char *argv[])
{
  struct evaluator *ev;
  char modules_dir[1024];
  snprintf(modules_dir, sizeof(modules_dir), "%s/../modules", argv[0]);
  if (!find_modules_dir(argv[0], modules_dir))
  {
    fprintf(stderr, "Could not find modules directory\n");
    exit(1);
  }
  ev = evaluator_create();
  if (!ev)
  {
    error("Cannot initialize Lisper");
    exit(1);
  }
  struct macros_expander *expander = macros_create(ev->machine);
  load_modules(ev, expander, modules_dir);
  info("Lisper initialized");
  add_primitives(ev->machine, list(ev->machine, mk_sym("say-hello"), NIL),
                 list(ev->machine, mk_prim(prim_say_hello), NIL));

  if (argc > 1)
  {
    evaluate_file(ev, expander, argv[1]);
  }
  else
    repl(ev, expander, argc, argv);

  // Print detailed stats
  memory_print_stats(ev->memory);

  macros_destroy(expander);
  evaluator_destroy(ev);
  return 0;
}
