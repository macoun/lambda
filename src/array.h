#ifndef stack_h
#define stack_h

#include "cell.h"
#include <stdlib.h>
#include <stdbool.h>

struct array
{
  struct cell *cells; // Array of cells for stack storage
  size_t size;        // Current stack pointer (number of items)
  size_t capacity;    // Maximum capacity of stack
};

// Creation/destruction
struct array *array_create(size_t initial_capacity);
struct array *array_copy(const struct array *src, bool compact);
void array_destroy(struct array *col);

// Operations
void array_push(struct array *col, struct cell value);
void array_pushn(struct array *col, struct cell *first, long n);
struct cell array_pop(struct array *col);
struct cell *array_peek(struct array *col, int depth);
size_t array_size(const struct array *col);

#define array_full(s) ((s)->size >= (s)->capacity)
#define array_empty(s) ((s)->size == 0)

#endif