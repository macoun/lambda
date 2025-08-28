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

static struct cell relocate(struct memory *mem, struct cell *oldc)
{
  if (!is_compound(*oldc))
    return *oldc;
  if (has_moved(oldc->array[0]))
    return mk_cell(oldc->type, oldc->array[1].array);

  long size = (is_pair(*oldc)) ? 2 : (oldc->array[0].uintv + 1);
  struct cell *array = memory_alloc(mem, size, false);
  struct cell newcell = mk_cell(oldc->type, array);

  for (long i = 0; i < size; i++)
    newcell.array[i] = oldc->array[i];

  oldc->array[0].type = MOVED;
  oldc->array[1].array = newcell.array;

  return newcell;
}

static bool relocate_objects(struct memory *mem, struct array *root)
{
  long i;
  for (i = 0; i < root->size; i++)
  {
    root->cells[i] = relocate(mem, &root->cells[i]);
  }
  return true;
}

void gc(struct memory *mem)
{
  // Mark all objects in root set as reachable (copy to inactive space)
  memory_scan(mem, (memory_callback_f)relocate_objects, MEMSPACE_ROOTS);

  // Transitively copy all objects reachable from inactive space
  memory_scan(mem, (memory_callback_f)relocate_objects, MEMSPACE_INACTIVE);

  // Flip memory spaces - now inactive (with live objects) becomes active
  memory_flip(mem);
}
