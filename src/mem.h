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
#include "cell.h"
#include "segment.h"

struct memory
{
  struct segment *segments;
  struct segment *inactive;
  struct segment *freesegs;
  struct segment *roots;

  long created_segment_count;
};

enum memspace
{
  MEMSPACE_ACTIVE,
  MEMSPACE_INACTIVE,
  MEMSPACE_ROOTS
};

typedef bool (*memory_callback_f)(struct memory *, struct array *);

long memory_active_object_count(struct memory *mem);

struct memory *memory_create();
void memory_destroy(struct memory *mem);
void memory_flip(struct memory *mem);
struct cell *memory_alloc(struct memory *mem, long s, enum memspace space);
void memory_scan(struct memory *mem, memory_callback_f callback, enum memspace space);
struct array *memory_alloc_root(struct memory *mem, long s);

#endif /* mem_h */
