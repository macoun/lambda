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
#include <stdbool.h>
#include <time.h>
#include "cell.h"
#include "segment.h"

// GC statistics tracking
struct gc_stats
{
  long collections;
  long objects_collected;
  long bytes_collected;
  clock_t total_gc_time_ms;
  clock_t last_gc_time_ms;
  double avg_collection_rate;
};

// Memory manager structure
struct memory
{
  struct segment *segments;
  struct segment *inactive;
  struct segment *freesegs;
  struct segment *roots;

  long created_segment_count;
  struct gc_stats *gc_stats;
};

enum memspace
{
  MEMSPACE_ACTIVE,
  MEMSPACE_INACTIVE,
  MEMSPACE_ROOTS
};

// Callback function type for memory scanning
typedef bool (*memory_callback_f)(struct memory *, struct array *);

// Memory lifecycle functions
struct memory *memory_create(void);
void memory_destroy(struct memory *mem);

// Memory allocation functions
struct cell *memory_alloc(struct memory *mem, long s, bool active);
struct array *memory_alloc_root(struct memory *mem, long s);

// Memory utility functions
void memory_scan(struct memory *mem, memory_callback_f callback, enum memspace space);
void memory_flip(struct memory *mem);
long memory_active_object_count(struct memory *mem);

// GC statistics functions
void memory_print_stats(struct memory *mem);

#endif /* mem_h */
