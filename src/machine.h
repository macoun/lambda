#ifndef machine_h
#define machine_h

#include "array.h"

struct machine
{
    struct array *stack;
    struct array *registers;
    struct memory *memory;
};

struct machine *machine_create(struct memory *mem, int register_size, long stack_size);
void machine_destroy(struct machine *m);
void machine_push(struct machine *m, struct cell value);
void machine_pop(struct machine *m, struct cell *value);

#define push(e) machine_push(ev->machine, e)
#define pop(e) machine_pop(ev->machine, &e)

#endif /* machine_h */
