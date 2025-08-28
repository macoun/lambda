#include "machine.h"
#include "mem.h"
#include <stdlib.h>

/*-----------------------------------------------------------------------*/
/* Init/Destroy                                                          */
/*-----------------------------------------------------------------------*/
struct machine *machine_create(struct memory *mem, int register_size, long stack_size)
{
    struct machine *m = malloc(sizeof(struct machine));
    if (!m)
        return NULL;
    m->stack = memory_alloc_root(mem, stack_size);
    if (!m->stack)
    {
        free(m);
        return NULL;
    }

    m->registers = memory_alloc_root(mem, 7);
    if (!m->registers)
    {
        free(m);
        return NULL;
    }
    for (int i = 0; i < register_size; i++)
        array_push(m->registers, NIL);

    m->memory = mem;
    return m;
}

void machine_destroy(struct machine *m)
{
    if (m)
    {
        free(m);
    }
}

/*-----------------------------------------------------------------------*/
/* Stack management                                                      */
/*-----------------------------------------------------------------------*/
inline void machine_push(struct machine *m, struct cell value)
{
    array_push(m->stack, value);
}

inline void machine_pop(struct machine *m, struct cell *value)
{
    *value = array_pop(m->stack);
}

inline void machine_push_reg(struct machine *m, int reg)
{
    machine_push(m, m->registers->cells[reg]);
}

inline void machine_pop_reg(struct machine *m, int reg)
{
    machine_pop(m, &m->registers->cells[reg]);
}

inline void machine_mov_reg(struct machine *m, int srcreg, int destreg)
{
    m->registers->cells[destreg] = m->registers->cells[srcreg];
}

inline void machine_set_reg(struct machine *m, int reg, struct cell val)
{
    m->registers->cells[reg] = val;
}

inline struct cell machine_get_reg(struct machine *m, int reg)
{
    return m->registers->cells[reg];
}
