//
//  repl.c
//  Lisper
//
//  Created by Ferhat Ayaz on 21/03/16.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#include "repl.h"
#include "reader.h"
#include "printer.h"
#include "evaluator.h"

#include <stdio.h>

#define MAX_LINE_SIZE 1024

#if 0
  #define COLOR_CONSOLE
  #define COLOR_INPUT
  #define COLOR_OUTPUT
  #undef COLOR_RESET
  #define COLOR_RESET
#else
  #define COLOR_CONSOLE COLOR_YELLOW
  #define COLOR_INPUT COLOR_MAGENTA
  #define COLOR_OUTPUT COLOR_GREEN
#endif

// You must call lisper_init() first!
void repl(int argc, const char *argv[])
{
  char buffer[MAX_LINE_SIZE], *s;
  char stop;
  expr exp;
  int error;

  eprintf(COLOR_CONSOLE, "Lambda REPL " __REPL_VERSION__ "\n");

  stop = 0;
  while (!stop)
  {
    fprintf(stdout, COLOR_CONSOLE "> " COLOR_RESET "");
    s = fgets(buffer, MAX_LINE_SIZE, stdin);
    s = &buffer[0];
    while (*s != '\0')
    {
      exp = parse_exp(&s, &error);
      if (error)
        break;
      push(exp);
      exp = eval(exp);

      fprintf(stdout, COLOR_OUTPUT "");
      print_exp(exp);
      fprintf(stdout, COLOR_RESET "\n");

      pop(exp);
    }
  }
}
