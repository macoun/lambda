//
//  mem.h
//  Lisper
//
//  Created by Ferhat Ayaz on 13/03/2016.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#ifndef mem_h
#define mem_h

#include <stdlib.h>

extern const long MACHINE_SIZE;
extern const long REGISTER_COUNT;

enum cell_type
{
  MOVED = 1,
  INTEGER = 2,
  FLOAT = 3,
  SYMBOL = 4,
  STRING = 5,
  PAIR = 6,
  VECTOR = 7,
  PRIMITIVE = 8,
  CUSTOM = 9
};

struct cell
{
  enum cell_type type;
  union
  {
    void *value;
    char *str;
    struct cell *array;
    int intv;
    long longv;
    unsigned int uintv;
    unsigned long ulongv;
  };
};

extern const struct cell NIL;

int mem_init(void);
struct cell *mem_new(long s);
size_t mem_array_size(struct cell c);

long mem_get_stackptr(void);
void mem_set_stackptr(long sp);

void mem_push(struct cell c);
void mem_pop(struct cell *c);

#define push(e) mem_push(e)
#define pop(e) mem_pop(&e)

#ifdef COLORED
  #define COLOR_RED     "\x1b[31m"
  #define COLOR_GREEN   "\x1b[32m"
  #define COLOR_YELLOW  "\x1b[33m"
  #define COLOR_BLUE    "\x1b[34m"
  #define COLOR_MAGENTA "\x1b[35m"
  #define COLOR_CYAN    "\x1b[36m"
  #define COLOR_RESET   "\x1b[0m"
#else
  #define COLOR_RED     ""
  #define COLOR_GREEN   ""
  #define COLOR_YELLOW  ""
  #define COLOR_BLUE    ""
  #define COLOR_MAGENTA ""
  #define COLOR_CYAN    ""
  #define COLOR_RESET   ""
#endif

#if 0
#define eprintf(color, format,...) \
  fprintf (stderr, color format COLOR_RESET "\n", ##__VA_ARGS__)
#else
#define eprintf(color, format,...) \
  fprintf (stderr, format "\n", ##__VA_ARGS__)
#endif

#define error(...) eprintf(COLOR_RED, __VA_ARGS__)
#define info(...) eprintf(COLOR_GREEN, __VA_ARGS__)

#endif /* mem_h */
