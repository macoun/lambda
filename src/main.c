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

int evaluate_file(struct evaluator *ev, const char *fname)
{
  char *buffer, *p;
  expr exp;
  int err;

  buffer = read_file(fname);
  p = buffer;
  for (; *p != '\0';)
  {
    exp = parse_exp(ev->machine, &p, &err);
    if (err == PERR_FIN)
      break;

    if (err != 0)
    {
      error("Error: %d\n", err);
      break;
    }

    push(exp);

    eval(ev, exp);

    pop(exp);
  }

  free(buffer);
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
  add_primitives(ev->machine, list(ev->machine, mk_sym("say-hello")),
                 list(ev->machine, mk_prim(prim_say_hello)));

  if (argc > 1)
  {
    evaluate_file(ev, argv[1]);
  }
  else
    repl(ev, argc, argv);

  evaluator_destroy(ev);
  return 0;
}
