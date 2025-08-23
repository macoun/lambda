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

long mem_get_stackptr(void);
void mem_set_stackptr(long sp);

void mem_push(struct cell c);
void mem_pop(struct cell *c);

void push_registers(void);
void pop_registers(void);

#define push(e) mem_push(e)
#define pop(e) mem_pop(&e)

#define vect_set(vect, idx, item) \
  do                              \
  {                               \
    if (vect.array)               \
      vect.array[idx + 1] = item; \
  } while (0)

#define vect_get(vect, idx) \
  ((vect.array) ? vect.array[idx + 1] : NIL)

#define vect_size(vect) \
  ((vect.array) ? vect.array[0].longv : 0)

#define is_nil(e) (e.type == 0 && e.array == NULL)
#define is_pair(e) (e.type == PAIR)
#define is_sym(e) (e.type == SYMBOL)
#define is_str(e) (e.type == STRING)
#define is_num(e) (e.type == INTEGER || e.type == FLOAT)
#define is_prim(e) (e.type == PRIMITIVE)
#define is_custom(e) (e.type == CUSTOM)
#define is_vector(e) (e.type == VECTOR)

#define is_eq(a, b) (a->value == b.value)

#define mk_cell(t, v) ((struct cell){t, {v}})
#define mk_num(num) mk_cell(INTEGER, (void *)num)
#define mk_str(str) mk_cell(STRING, strdup(str))
#define mk_sym(str) mk_cell(SYMBOL, strdup(str))
#define mk_prim(func) mk_cell(PRIMITIVE, func)

#define COLOR_RED "\x1b[31m"
#define COLOR_GREEN "\x1b[32m"
#define COLOR_YELLOW "\x1b[33m"
#define COLOR_BLUE "\x1b[34m"
#define COLOR_MAGENTA "\x1b[35m"
#define COLOR_CYAN "\x1b[36m"
#define COLOR_RESET "\x1b[0m"

#if COLORED
#define eprintf(color, format, ...) \
  fprintf(stderr, color format COLOR_RESET "\n", ##__VA_ARGS__)
#else
#define eprintf(color, format, ...) \
  fprintf(stderr, format "\n", ##__VA_ARGS__)
#endif

#define error(...) eprintf(COLOR_RED, __VA_ARGS__)
#define info(...) eprintf(COLOR_GREEN, __VA_ARGS__)

#endif /* mem_h */
