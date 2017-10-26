//
//  gc.c
//  Lisper
//
//  Created by Ferhat Ayaz on 14/03/16.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#include "gc.h"

#if 0

#pragma mark GC

static cont_f __continue;
static cont_f __goto;

static size_t __free, __scan;
static struct exp *__root;
static struct exp *__old, *__new;

static void begin_garbage_collection();
static void reassign_root();
static void gc_loop();
static void update_car();
static void update_cdr();
static void relocate_old_result_in_new();
static void pair();
static void already_moved();
static void gc_flip();

static struct exp __registers;

void gc()
{
  size_t before, after;
  
  printf("Starting garbage collection...\n");
  running_gc = 1;
  
  __registers = get_registers();
  before = __mem->cur;
  __root = &__registers;
  __continue = NULL;
  __goto = begin_garbage_collection;
  while (__goto)
  {
    __goto();
  }
  
  after = __mem->cur;
  running_gc = 0;
  printf("Removed %ld objects.\n", before - after);
}

static void reassign_root()
{
  __root = __new;
  __goto = gc_loop;
}

static void begin_garbage_collection()
{
  __free = __scan = 0;
  __old = __root;
  __continue = reassign_root;
  __goto = relocate_old_result_in_new;
}

static void gc_loop()
{
  if (__scan >= __free)
  {
    __goto = gc_flip;
  }
  else
  {
    __old = __to_mem->cells + __scan;
    __continue = update_car;
    __goto = relocate_old_result_in_new;
  }
}

static void update_car()
{
  __to_mem->cells[__scan] = *__new; // car
  __scan += 1;
  __old = __to_mem->cells + __scan; // cdr
  __continue = update_cdr;
  __goto = relocate_old_result_in_new;
}

static void update_cdr()
{
  __to_mem->cells[__scan] = *__new;
  __scan += 1;
  __goto = gc_loop;
}

static void relocate_old_result_in_new()
{
  struct exp old = *__old;
  //  printf("relocate\n");
  //  fflush(stdout);
  if (__old->type == kExpTypeBrokenHeart)
  {
    __goto = already_moved;
  }
  else if (is_pair(old))
  {
    __goto = pair;
  }
  else
  {
    __to_mem->cells[__free] = old;
    __new = __to_mem->cells + __free;
    __goto = __continue;
  }
}

static void pair()
{
  if (__old->type == kExpTypeBrokenHeart)
  {
    __goto = already_moved;
  }
  else
  {
    // Copy the car and cdr to new memory
    __new = __to_mem->cells + __free;
    __to_mem->cells[__free] = __old->e[0];
    __to_mem->cells[__free + 1] = __old->e[1];
    
    // Construct the broken heart
    __old->type = kExpTypeBrokenHeart;
    __old->v = __new;
    
    __free += 2;
    __goto = relocate_old_result_in_new;
  }
}

static void already_moved()
{
  __new = __old->v;
  __goto = __continue;
}

static void gc_flip()
{
  __mem = __to_mem;
  __to_mem = __from_mem;
  __from_mem = __mem;
  __mem->cur = __free;
  __goto = NULL;
}

#endif
