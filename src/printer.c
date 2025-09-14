//
//  printer.c
//  Lisper
//
//  Created by Ferhat Ayaz on 11/03/2016.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#include "printer.h"
#include "evaluator.h"
#include "logger.h"
#include <stdio.h>

static void print_list(expr exp)
{
  bool first = true;
  printf("(");
  while (!is_nil(exp))
  {
    if (first && is_sym(car(exp)))
    {
      printf(COLOR_RED "%s" COLOR_RESET, car(exp).str);
    }
    else
    {
      print_exp(car(exp));
    }
    if (!is_nil(cdr(exp)))
      printf(" ");
    exp = cdr(exp);
    first = false;
  }
  printf(")");
}

static void print_pair(expr exp)
{
  printf("(");
  print_exp(car(exp));
  printf(" . ");
  print_exp(cdr(exp));
  printf(")");
}

static void print_vector(expr exp)
{
  int i;

  printf("#(");
  for (i = 0; i < vect_size(exp); i++)
  {
    print_exp(vect_get(exp, i));
    if (i != vect_size(exp) - 1)
      printf(" ");
  }
  printf(")");
}

static void print_procedure(expr proc)
{
  printf("#<proc args: ");
  print_exp(procedure_params(proc));
  printf("\n       body: ");
  print_exp(procedure_body(proc));
  printf(">\n");
}

void print_exp(expr exp)
{
  if (is_str(exp))
  {
    printf(COLOR_YELLOW "\"%s\"" COLOR_RESET, exp.str);
  }
  else if (is_sym(exp))
  {
    printf("%s", exp.str);
  }
  else if (is_num(exp))
  {
    if (exp.type == FLOAT)
      printf(COLOR_MAGENTA "%f" COLOR_RESET, exp.doublev);
    else
      printf(COLOR_MAGENTA "%ld" COLOR_RESET, exp.longv);
  }
  else if (is_nil(exp))
  {
    printf(COLOR_BRIGHT_WHITE "#<null>" COLOR_RESET);
  }
  else if (is_prim(exp))
  {
    printf("#<primitive>");
  }
  else if (is_false(exp))
  {
    printf(COLOR_CYAN "#f" COLOR_RESET);
  }
  else if (is_real_true(exp))
  {
    printf(COLOR_CYAN "#t" COLOR_RESET);
  }
  else if (is_custom(exp))
  {
    printf("#<custom>");
  }
  else if (is_procedure(exp))
  {
    print_procedure(exp);
  }
  else if (is_vector(exp))
  {
    print_vector(exp);
  }
  else if (is_list(exp))
  {
    print_list(exp);
  }
  else if (is_pair(exp))
  {
    print_pair(exp);
  }
  else
  {
    printf("Unknown type: %d\n", exp.type);
  }
  fflush(stdout);
}
/*-----------------------------------------------------------------------*/
/* Printing and logging                                                  */
/*-----------------------------------------------------------------------*/
// static char *typenames[] = {
//     "NIL",
//     "MOVED",
//     "INTEGER",
//     "FLOAT",
//     "SYMBOL",
//     "STRING",
//     "PAIR",
//     "VECTOR",
//     "PRIMITIVE",
//     "CUSTOM"};
