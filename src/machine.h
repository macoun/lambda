#ifndef machine_h
#define machine_h

#include "array.h"

struct machine
{
  struct array *stack;
  struct array *registers;
  struct memory *memory;
};

#define DEFAULT_STACK_SIZE (long)(1024 * 1)

struct machine *machine_create(struct memory *mem, int register_size, long stack_size);
void machine_destroy(struct machine *m);

void machine_push(struct machine *m, struct cell value);
void machine_pop(struct machine *m, struct cell *value);
void machine_push_reg(struct machine *m, int reg);
void machine_pop_reg(struct machine *m, int reg);
void machine_mov_reg(struct machine *m, int srcreg, int destreg);
void machine_set_reg(struct machine *m, int reg, struct cell val);
struct cell machine_get_reg(struct machine *m, int reg);

struct cell machine_snapshot(struct machine *m);
bool machine_restore(struct machine *m, struct cell snapshot);

#endif /* machine_h */
