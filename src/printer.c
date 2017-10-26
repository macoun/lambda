//
//  printer.c
//  Lisper
//
//  Created by Ferhat Ayaz on 11/03/2016.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#include "printer.h"
#include "evaluator.h"
#include <stdio.h>

static void print_list(expr exp)
{
  printf("(");
  while (!is_nil(exp))
  {
    print_exp(car(exp));
    if (!is_nil(cdr(exp)))
      printf(" ");
    exp = cdr(exp);
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

static void _print_list(expr exp)
{
  if (is_nil(cdr(exp)))
  {
    print_exp(car(exp));
  }
  else if (is_pair(cdr(exp)))
  {
    print_exp(car(exp));
    printf(" ");
    print_list(cdr(exp));
  }
  else
  {
    print_exp(car(exp));
    printf(" . ");
    print_exp(cdr(exp));
  }
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
    printf("\"%s\"", exp.str);
  }
  else if (is_sym(exp))
  {
    printf("%s", exp.str);
  }
  else if (is_num(exp))
  {
    printf("%ld", exp.longv);
  }
  else if (is_nil(exp))
  {
    printf("#<null>");
  }
  else if (is_prim(exp))
  {
    printf("#<primitive>");
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
// else if (is_quote(exp))
//  {
//    printf("'");
//    print_exp(car(exp)); // text-of-quotation (quote_text)
//  }
  else if (is_list(exp))
  {
    print_list(exp);
  }
  else if (is_pair(exp))
  {
    print_pair(exp);
  }
//  else if (is_pair(exp))
//  {
//    printf("(");
//    print_list(exp);
//    printf(")");
//  }
  else
  {
    printf("Unknown type: %d\n", exp.type);
  }
}
