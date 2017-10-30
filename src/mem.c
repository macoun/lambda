//
//  mem.c
//  Lisper
//
//  Created by Ferhat Ayaz on 13/03/2016.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#include "mem.h"
#include <stdio.h>
#include <assert.h>

#include "evaluator.h"

static char *typenames[] =
{
  "NIL",
  "MOVED",
  "INTEGER",
  "FLOAT",
  "SYMBOL",
  "STRING",
  "PAIR",
  "VECTOR",
  "PRIMITIVE",
  "CUSTOM"
};


static long __free, __stackptr;

long mem_get_stackptr() { return __stackptr; }
void mem_set_stackptr(long sp) { __stackptr = sp; }

struct cell *__back;
struct cell *__front;

const long MACHINE_SIZE = 1024*10;
const long REGISTER_COUNT = 0;
const long GLUE_SIZE = 20;
const struct cell NIL = mk_cell(0, NULL);

void gc(void);
void print_mem(struct cell *mem)
{
  struct cell c;
  long i;
  long ref;
  
  for (i = 0; i < MACHINE_SIZE; i++)
  {
    c = mem[i];
    printf("%5ld [%c]%s", i,
           (i < __stackptr) ? ((i < __free) ? 'D' : ((i == __free) ? '+' : 'F')) :
            ((i == __stackptr) ? '-' : 'S') ,
           typenames[c.type]);
    if (is_pair(c))
    {
      ref = (c.array - mem)/sizeof(struct cell*);
      printf(" -> %ld", ref);
    }
    printf("\n");
  }
}

#define hard_limit_reached (__stackptr < __free + GLUE_SIZE)
#define has_moved(e) (e.type == MOVED)

// Stack
void mem_push(struct cell c)
{
  // ! could overflow the glue area
  __front[__stackptr--] = c; 
  
  if (hard_limit_reached)
  {
    gc();
  }
}

void mem_pop(struct cell *c)
{
  if (__stackptr >= MACHINE_SIZE)
  {
    fprintf(stderr, "Cannot pop on empty stack\n");
    exit(1);
  }
  *c = __front[++__stackptr];
}


int mem_init()
{
  printf("Machine Size is %ld\n", MACHINE_SIZE);
  __front = (struct cell *)calloc(MACHINE_SIZE, sizeof(struct cell));
  __back = (struct cell *)calloc(MACHINE_SIZE, sizeof(struct cell));
  __stackptr = MACHINE_SIZE - REGISTER_COUNT - 1;
  __free = 0;
  return (__back) && (__front);
}

struct cell *mem_new(long s)
{

  if (__free + GLUE_SIZE + s >= __stackptr)
  {
//    printf("Out of memory\n");
    gc();
    return mem_new(s);
  }
  else
  {
    __free += s;
    return __front + __free - s;
  }
  return 0;
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

#pragma mark GC

void push_registers()
{
  int i;
  for (i = 0; i <= 6; i++)
    __front[__stackptr--] = *__registers[i];
}

void pop_registers()
{
  int i;
  for (i = 6; i >= 0; i--)
    *__registers[i] = __front[++__stackptr];
}

void print_mem_contents()
{
#if 0
  printf("FRONT %0.*d\n", 20, 0);
  print_mem(__front);
  printf("BACK %0.*d\n", 20, 0);
//  print_mem(__back);
#endif
}


struct cell relocate_pair(long old)
{
  struct cell *oldc;
  struct cell pair;
  long idx;
  
  pair.type = PAIR;
  oldc = __back + old;
  
  if (has_moved(oldc->array[0]))
  {
    pair.array = oldc->array[1].array;
  }
  else
  {
    idx = (oldc->array - __front)/sizeof(struct cell*);
    if (idx < __stackptr)
    {
      idx = __free;
      __free += 2;
      if (hard_limit_reached)
      {
        error("Not enough memory");
        exit(1);
      }
    }
    __back[idx] = oldc->array[0];
    __back[idx + 1] = oldc->array[1];
    
    oldc->array[0].type = MOVED;
    oldc->array[1].array = __back + idx;
    
    pair.array = __back + idx;
  }
  
  return pair;
}

struct cell relocate_vector(long old)
{
  struct cell *oldc;
  struct cell vect;
  long idx, size, i;
  
  vect.type = VECTOR;
  oldc = __back + old;
  
  if (has_moved(oldc->array[0]))
  {
    vect.array = oldc->array[1].array;
  }
  else
  {
    idx = (oldc->array - __front)/sizeof(struct cell*);
    size = mem_array_size(*oldc);
    
    if (idx < __stackptr)
    {
      idx = __free;
      __free += size + 1;
      if (hard_limit_reached)
      {
        error("Not enough memory");
        exit(1);
      }
    }
    for (i = 0; i < size + 1; i++)
    __back[idx + i] = oldc->array[i];
    
    oldc->array[0].type = MOVED;
    oldc->array[1].array = __back + idx;
    
    vect.array = __back + idx;
  }
  
  return vect;
}

struct cell relocate(long old)
{
  if (is_pair(__back[old]))
    return relocate_pair(old);
  if (is_vector(__back[old]))
    return relocate_vector(old);
  return __back[old];
}

void gc_run(void);
static void gc_flip(void);

static void check_mem_refs()
{
  int i;
  
  for (i = 0; i < __free; i++)
  {
    if (is_pair(__front[i]))
    {
      if ((__front[i].array - __front)/sizeof(struct cell*) >= MACHINE_SIZE)
      {
        assert(0);
      }
    }
  }
}

void gc()
{
  long before, sp;
  
  before = __free;
  sp = __stackptr;
  push_registers();
  print_mem_contents();
  gc_run();
  gc_flip();
  print_mem_contents();
  pop_registers();
  
  check_mem_refs();
  
  assert(sp == __stackptr);
  // info("Purged %ld objects\n", before - __free);
  fflush(stdout);
}

void gc_run()
{
  long scan;
  
  __free = 0;
  for (scan = __stackptr + 1; scan < MACHINE_SIZE; scan++)
    __back[scan] = __front[scan];
  
  for (scan = __stackptr + 1; scan < MACHINE_SIZE; scan++)
    __back[scan] = relocate(scan);
  
  for (scan = 0; scan < __free; scan++)
    __back[scan] = relocate(scan);
}

static void gc_flip()
{
  struct cell *tmp;
  tmp = __back;
  __back = __front;
  __front = tmp;
}

