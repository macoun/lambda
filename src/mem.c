//
//  mem.c
//  Lisper
//
//  Created by Ferhat Ayaz on 13/03/2016.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#include "mem.h"
#include "evaluator.h"

#include <stdio.h>
#include <assert.h>
#include <time.h>
#include <stdbool.h>

const struct cell NIL = mk_cell(0, NULL);

const long DEFAULT_SEGMENT_SIZE = 1024 * 10;
const long DEFAULT_STACK_SIZE = 1024 * 1;

#define has_moved(e) (e.type == MOVED)

/*-----------------------------------------------------------------------*/
/* Memory segmentation                                                   */
/*-----------------------------------------------------------------------*/
struct segment
{
  struct cell *cells;
  long size;
  long capacity;
  struct segment *next;
};

struct segment *__front = NULL;
struct segment *__segpool = NULL;
struct segment *__stack = NULL;
long __created_segments = 0;

#define segment_full(s) ((s)->size >= (s)->capacity)
#define segment_can_add(s, n) ((s)->size + (n) < (s)->capacity)

struct segment *segment_create(long size)
{
  if (__segpool)
  {
    struct segment *seg = __segpool;
    __segpool = __segpool->next;
    seg->next = NULL;
    seg->size = 0;
    return seg;
  }

  struct segment *seg = (struct segment *)malloc(sizeof(struct segment));
  if (!seg)
  {
    fprintf(stderr, "Memory allocation failed for segment\n");
    exit(1);
  }
  seg->cells = (struct cell *)calloc(size, sizeof(struct cell));
  if (!seg->cells)
  {
    fprintf(stderr, "Memory allocation failed for segment cells\n");
    free(seg);
    exit(1);
  }
  seg->size = 0;
  seg->capacity = size;
  seg->next = NULL;
  __created_segments++;
  return seg;
}

long segment_sum_sizes(struct segment *seg)
{
  long total = 0;
  while (seg)
  {
    total += seg->size;
    seg = seg->next;
  }
  return total;
}

/*-----------------------------------------------------------------------*/
/* Memory init and allocation                                            */
/*-----------------------------------------------------------------------*/
void gc(void);
int mem_init()
{
  __front = segment_create(DEFAULT_SEGMENT_SIZE);
  __stack = segment_create(DEFAULT_STACK_SIZE);

  return (__front) && (__stack);
}

struct cell *mem_new(long s)
{
  struct segment *seg;

  if (!segment_can_add(__front, s))
  {
    gc();
    if (!segment_can_add(__front, s))
    {
      seg = segment_create(DEFAULT_SEGMENT_SIZE);
      if (!seg)
      {
        fprintf(stderr, "Out of memory.\n");
        exit(1);
      }
      seg->next = __front;
      __front = seg;
    }
  }
  __front->size += s;
  return __front->cells + __front->size - s;
}

size_t mem_array_size(struct cell c)
{
  if (is_pair(c))
    return 2;
  else if (is_vector(c))
    return c.array[0].uintv;
  else if (c.array && has_moved(c.array[0]))
    return c.array[1].array->uintv;

  return 0;
}

/*-----------------------------------------------------------------------*/
/* Stack management                                                      */
/*-----------------------------------------------------------------------*/
long mem_get_stackptr() { return __stack->size; }
void mem_set_stackptr(long sp) { __stack->size = sp; }

// Stack
void mem_push(struct cell c)
{
  if (segment_full(__stack))
  {
    fprintf(stderr, "Stack overflow\n");
    exit(1);
  }
  __stack->cells[__stack->size++] = c;
}

void mem_pop(struct cell *c)
{
  if (__stack->size <= 0)
  {
    fprintf(stderr, "Cannot pop on empty stack\n");
    exit(1);
  }
  *c = __stack->cells[--__stack->size];
}

void push_registers()
{
  int i;
  for (i = 0; i <= 6; i++)
    __stack->cells[__stack->size++] = *__registers[i];
}

void pop_registers()
{
  int i;
  for (i = 6; i >= 0; i--)
    *__registers[i] = __stack->cells[--__stack->size];
}

/*-----------------------------------------------------------------------*/
/* Garbage collection                                                    */
/*-----------------------------------------------------------------------*/
struct segment *gc_run(void);
static void gc_flip(struct segment *back);
struct cell relocate(struct cell *oldc, struct segment *dest);
struct cell relocate_vector(struct cell *oldc, struct segment *dest);
struct cell relocate_pair(struct cell *oldc, struct segment *dest);

struct cell relocate_pair(struct cell *oldc, struct segment *dest)
{
  struct segment *seg;
  struct cell pair;
  long idx;

  pair.type = PAIR;

  if (has_moved(oldc->array[0]))
  {
    pair.array = oldc->array[1].array;
  }
  else
  {
    if (!segment_can_add(dest, 2))
    {
      seg = segment_create(DEFAULT_SEGMENT_SIZE);
      if (!seg)
      {
        error("Not enough memory to copy pair.");
        exit(1);
      }
      dest->next = seg;
      dest = seg;
    }

    idx = dest->size;
    dest->size += 2;
    dest->cells[idx] = oldc->array[0];
    dest->cells[idx + 1] = oldc->array[1];

    oldc->array[0].type = MOVED;
    oldc->array[1].array = dest->cells + idx;

    pair.array = dest->cells + idx;
  }

  return pair;
}

struct cell relocate_vector(struct cell *oldc, struct segment *dest)
{
  struct cell vect;
  struct segment *seg;
  long idx, size, i;

  vect.type = VECTOR;

  if (has_moved(oldc->array[0]))
  {
    vect.array = oldc->array[1].array;
  }
  else
  {
    size = mem_array_size(*oldc);

    if (!segment_can_add(dest, size + 1))
    {
      seg = segment_create(DEFAULT_SEGMENT_SIZE);
      if (!seg)
      {
        error("Not enough memory to copy vector.");
        exit(1);
      }
      dest->next = seg;
      dest = seg;
    }

    idx = dest->size;
    dest->size += size + 1;

    for (i = 0; i < size + 1; i++)
      dest->cells[idx + i] = oldc->array[i];

    oldc->array[0].type = MOVED;
    oldc->array[1].array = dest->cells + idx;

    vect.array = dest->cells + idx;
  }

  return vect;
}

struct cell relocate(struct cell *oldc, struct segment *dest)
{
  if (is_pair((*oldc)))
    return relocate_pair(oldc, dest);
  if (is_vector((*oldc)))
    return relocate_vector(oldc, dest);
  return *oldc;
}

clock_t get_time_ms();

void gc()
{
  long before, after;
  clock_t start_time, end_time;
  struct segment *back;

  // Record pre-GC metrics
  before = segment_sum_sizes(__front);
  start_time = get_time_ms();

  push_registers();
  back = gc_run();
  gc_flip(back);
  pop_registers();

  // Calculate GC duration
  end_time = get_time_ms();
  after = segment_sum_sizes(__front);
  fprintf(stdout, "Purged %ld objects in %ld ms [%ld segments]\n",
          before - after, end_time - start_time, __created_segments);

  fflush(stdout);
}

struct segment *gc_run()
{
  long scan;
  struct segment *seg, *scanseg, *head;

  seg = segment_create(DEFAULT_SEGMENT_SIZE);
  if (!seg)
  {
    fprintf(stderr, "Out of memory.\n");
    exit(1);
  }

  head = seg;
  for (scan = 0; scan < __stack->size; scan++)
  {
    __stack->cells[scan] = relocate(&__stack->cells[scan], seg);
    if (seg->next)
      seg = seg->next;
  }

  scanseg = head;
  while (scanseg)
  {
    for (scan = 0; scan < scanseg->size; scan++)
    {
      scanseg->cells[scan] = relocate(&scanseg->cells[scan], seg);
      if (seg->next)
        seg = seg->next;
    }
    scanseg = scanseg->next;
  }

  return head;
}

static void gc_flip(struct segment *back)
{
  struct segment *seg;
  seg = __front;
  while (seg->next)
    seg = seg->next;
  seg->next = __segpool;
  __segpool = __front;
  __front = back;
}

// Get current time in milliseconds
clock_t get_time_ms()
{
  return clock() * 1000 / CLOCKS_PER_SEC;
}
