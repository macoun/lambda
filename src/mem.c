//
//  mem.c
//  Lisper
//
//  Created by Ferhat Ayaz on 13/03/2016.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#include "mem.h"
#include "logger.h"
#include "array.h"
#include "gc.h"

#include <stdio.h>
#include <assert.h>
#include <time.h>
#include <stdbool.h>

const struct cell NIL = mk_cell(0, NULL);

/*-----------------------------------------------------------------------*/
/* Memory segmentation                                                   */
/*-----------------------------------------------------------------------*/
const long SEGMENT_MAX_IDLE_COUNT = 2;

static struct segment *memory_segment_create(struct memory *mem, long size, enum memspace space)
{
  struct segment *seg = NULL;

  if (!mem)
  {
    fprintf(stderr, "Memory is NULL\n");
    return NULL;
  }

  if (mem->freesegs)
  {
    seg = mem->freesegs;
    mem->freesegs = mem->freesegs->next;
    seg->data->size = 0;
    seg->idle_count = 0;
    seg->recycle_count++;
  }
  else
  {
    seg = segment_create(size);
    if (!seg)
    {
      fprintf(stderr, "Memory allocation failed for segment\n");
      exit(1);
    }
    mem->created_segment_count++;

    fprintf(stdout, "Creating new segment of size %ld [%ld]\n", size, mem->created_segment_count);
  }

  if (space == MEMSPACE_ACTIVE)
  {
    seg->next = mem->segments;
    mem->segments = seg;
  }
  else if (space == MEMSPACE_INACTIVE)
  {
    seg->next = NULL;
    if (!mem->inactive)
    {
      mem->inactive = seg;
    }
    else
    {
      struct segment *tail = mem->inactive;
      while (tail->next)
        tail = tail->next;
      tail->next = seg;
    }
  }
  else if (space == MEMSPACE_ROOTS)
  {
    seg->next = mem->roots;
    mem->roots = seg;
  }
  else
  {
    fprintf(stderr, "Unknown memory space %d\n", space);
    exit(1);
  }

  return seg;
}

/*-----------------------------------------------------------------------*/
/* Memory lifecyle                                                       */
/*-----------------------------------------------------------------------*/
struct memory *memory_create()
{
  struct memory *mem = (struct memory *)malloc(sizeof(struct memory));
  if (!mem)
  {
    fprintf(stderr, "Memory allocation failed for memory struct\n");
    exit(1);
  }
  fprintf(stdout, "Creating memory...\n");
  mem->created_segment_count = 0;
  mem->segments = memory_segment_create(mem, DEFAULT_SEGMENT_SIZE, MEMSPACE_ACTIVE);
  if (!mem->segments)
  {
    fprintf(stderr, "Memory allocation failed for initial segment\n");
    free(mem);
    exit(1);
  }
  mem->roots = NULL;
  mem->inactive = NULL;
  mem->freesegs = NULL;
  fprintf(stdout, "Created initial memory segment of size %ld\n", (long)DEFAULT_SEGMENT_SIZE);
  fflush(stdout);
  return mem;
}

void memory_destroy(struct memory *m)
{
  struct segment *seg, *next;

  if (m)
  {
    seg = m->segments;
    while (seg)
    {
      next = seg->next;
      segment_destroy(seg);
      seg = next;
    }

    seg = m->freesegs;
    while (seg)
    {
      next = seg->next;
      segment_destroy(seg);
      seg = next;
    }

    seg = m->roots;
    while (seg)
    {
      next = seg->next;
      segment_destroy(seg);
      seg = next;
    }

    free(m);
  }
}

static void memory_destroy_idle_segments(struct memory *mem)
{
  struct segment *seg, *prev, *next;

  prev = NULL;
  seg = mem->freesegs;

  while (seg)
  {
    if (seg->idle_count > SEGMENT_MAX_IDLE_COUNT)
    {
      next = seg->next;
      if (prev)
        prev->next = next;
      if (mem->freesegs == seg)
        mem->freesegs = next;
      fprintf(stdout, "Freeing segment from pool %p (recycle %d)\n", (void *)seg, seg->recycle_count);
      segment_destroy(seg);
      seg = next;
    }
    else
    {
      seg->idle_count++;
      prev = seg;
      seg = seg->next;
    }
  }
}
/*-----------------------------------------------------------------------*/
/* Memory allocation                                                     */
/*-----------------------------------------------------------------------*/
struct array *memory_alloc_root(struct memory *mem, long s)
{
  struct segment *seg;

  seg = memory_segment_create(mem, s, MEMSPACE_ROOTS);
  if (!seg)
  {
    fprintf(stderr, "Out of memory.\n");
    exit(1);
  }
  return seg->data;
}

struct cell *memory_alloc(struct memory *mem, long s, enum memspace space)
{
  if (space == MEMSPACE_ACTIVE)
  {
    if (!segment_can_add(mem->segments, s))
    {
      gc(mem);
      if (!segment_can_add(mem->segments, s))
      {
        if (!memory_segment_create(mem, DEFAULT_SEGMENT_SIZE, space))
        {
          fprintf(stderr, "Out of memory.\n");
          exit(1);
        }
      }
    }
    mem->segments->data->size += s;
    return mem->segments->data->cells + mem->segments->data->size - s;
  }
  else if (space == MEMSPACE_INACTIVE)
  {
    struct segment *seg = mem->inactive;

    if (!seg)
    {
      seg = memory_segment_create(mem, DEFAULT_SEGMENT_SIZE, space);
      if (!seg)
      {
        fprintf(stderr, "Out of memory.\n");
        exit(1);
      }
      mem->inactive = seg;
    }
    else
    {
      while (seg->next)
        seg = seg->next;
    }

    if (!segment_can_add(seg, s))
    {
      seg = memory_segment_create(mem, DEFAULT_SEGMENT_SIZE, space);
      if (!seg)
      {
        fprintf(stderr, "Out of memory.\n");
        exit(1);
      }
    }
    seg->data->size += s;
    return seg->data->cells + seg->data->size - s;
  }
  else
  {
    fprintf(stderr, "Unknown memory space %d\n", space);
    exit(1);
  }
}

/*-----------------------------------------------------------------------*/
/* Memory utils                                                          */
/*-----------------------------------------------------------------------*/
void memory_scan(struct memory *mem, memory_callback_f callback, enum memspace space)
{
  struct segment *curroot;

  if (!callback)
    return;

  if (space == MEMSPACE_ACTIVE)
    curroot = mem->segments;
  else if (space == MEMSPACE_INACTIVE)
    curroot = mem->inactive;
  else if (space == MEMSPACE_ROOTS)
    curroot = mem->roots;
  else
    return;

  while (curroot)
  {
    if (!callback(mem, curroot->data))
      break;
    curroot = curroot->next;
  }
}

void memory_flip(struct memory *mem)
{
  struct segment *seg;

  memory_destroy_idle_segments(mem);

  seg = mem->segments;
  while (seg->next)
    seg = seg->next;
  seg->next = mem->freesegs;
  mem->freesegs = mem->segments;
  mem->segments = mem->inactive;
  mem->inactive = NULL;
}

long memory_active_object_count(struct memory *mem)
{
  return segment_count_active_objects(mem->segments);
}
