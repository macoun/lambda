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

#include "repl.h"

// You must call lisper_init() first!
int evaluate_file(const char *fname)
{
  char *buffer, *p;
  expr exp, res;
  int err;

  buffer = read_file(fname);
  p = buffer;
  for (; *p != '\0';)
  {
    exp = parse_exp(&p, &err);
    if (err == PERR_FIN)
      break;

    if (err != 0)
    {
      error("Error: %d\n", err);
      break;
    }

    push(exp);

    // printf("Parsed: ");
    // print_exp(exp);
    // printf("\n");
    // fflush(stdout);

    res = eval(exp);

    //    printf("Evaluated: ");
    //    print_exp(exp);
    //    printf("\n");

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

void test_lookup_name()
{
  expr *calc;
  expr res, cmd;

  calc = lookup_name("lj-potential");
  if (calc != NULL && is_procedure((*calc)))
  {
    cmd = list(mk_sym("lj-potential"), mk_num(4), NIL);
    res = eval(cmd);
    if (is_num(res))
      printf("lj-potential(4) = %ld\n", res.longv);
  }
}

int main(int argc, const char *argv[])
{
  if (!lisper_init())
  {
    error("Cannot initialize Lisper");
    exit(1);
  }

  add_primitives(list(mk_sym("say-hello")),
                 list(mk_prim(prim_say_hello)));

  if (argc > 1)
  {
    evaluate_file(argv[1]);
    // test_lookup_name();
  }
  else
    repl(argc, argv);

  return 0;
}

int main3(int argc, const char *argv[])
{
  if (!lisper_init())
  {
    error("Cannot initialize Lisper");
    exit(1);
  }

  if (argc > 1)
    return evaluate_file(argv[1]);
  else
    repl(argc, argv);

  return 0;
}
int main2(int argc, const char *argv[])
{
  const char *fname = "/Users/ayaz/Dropbox/Sources/Lisper/examples/test2.sc";
  int error;
  expr e;
  lisper_init();
  char *script = read_file(fname);
  char *p = script;
  e = parse_exp(&p, &error);
  print_exp(e);
  free(script);
  return 0;
}
