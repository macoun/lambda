//
//  gc.c
//  Lisper
//
//  Created by Ferhat Ayaz on 14/03/16.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#include "gc.h"
#include "logger.h"

#include <stdio.h>
#include <time.h>

void gc_run(struct memory *mem);
struct cell relocate(struct memory *mem, struct cell *oldc);
struct cell relocate_vector(struct memory *mem, struct cell *oldc);
struct cell relocate_pair(struct memory *mem, struct cell *oldc);

clock_t get_time_ms();

struct cell relocate_pair(struct memory *mem, struct cell *oldc)
{
  struct cell pair;

  pair.type = PAIR;

  if (has_moved(oldc->array[0]))
  {
    pair.array = oldc->array[1].array;
  }
  else
  {
    pair.array = memory_alloc(mem, 2, MEMSPACE_INACTIVE);
    pair.array[0] = oldc->array[0];
    pair.array[1] = oldc->array[1];
    oldc->array[0].type = MOVED;
    oldc->array[1].array = pair.array;
  }

  return pair;
}

struct cell relocate_vector(struct memory *mem, struct cell *oldc)
{
  struct cell vect;
  long size, i;

  vect.type = VECTOR;

  if (has_moved(oldc->array[0]))
  {
    vect.array = oldc->array[1].array;
  }
  else
  {
    size = oldc->array[0].uintv;
    vect.array = memory_alloc(mem, size + 1, MEMSPACE_INACTIVE);

    for (i = 0; i <= size; i++)
      vect.array[i] = oldc->array[i];

    oldc->array[0].type = MOVED;
    oldc->array[1].array = vect.array;
  }

  return vect;
}

struct cell relocate(struct memory *mem, struct cell *oldc)
{
  if (is_pair((*oldc)))
    return relocate_pair(mem, oldc);
  if (is_vector((*oldc)))
    return relocate_vector(mem, oldc);
  return *oldc;
}

void gc(struct memory *mem)
{
  long before, after;
  clock_t start_time, end_time;

  // Record pre-GC metrics
  before = memory_active_object_count(mem);
  start_time = get_time_ms();

  gc_run(mem);

  // Calculate GC duration
  end_time = get_time_ms();
  after = memory_active_object_count(mem);
  // fprintf(stdout, "Purged %ld objects in %ld ms [%ld segments]\n",
  //         before - after, end_time - start_time, mem->created_segment_count);

  fflush(stdout);
}

bool relocate_objects(struct memory *mem, struct array *root)
{
  long i;
  for (i = 0; i < root->size; i++)
  {
    root->cells[i] = relocate(mem, &root->cells[i]);
  }
  return true;
}

void gc_run(struct memory *mem)
{
  memory_scan(mem, (memory_callback_f)relocate_objects, MEMSPACE_ROOTS);
  memory_scan(mem, (memory_callback_f)relocate_objects, MEMSPACE_INACTIVE);

  memory_flip(mem);
}

// Get current time in milliseconds
clock_t get_time_ms()
{
  return clock() * 1000 / CLOCKS_PER_SEC;
}
