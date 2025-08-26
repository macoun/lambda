#ifndef segment_h
#define segment_h

#include "array.h"

#define DEFAULT_SEGMENT_SIZE 110 * 1

struct segment
{
    struct array *data;
    struct segment *next;
    int idle_count;
    int recycle_count;
};

#define segment_full(s) (array_full((s)->data))
#define segment_can_add(s, n) ((s)->data->size + (n) < (s)->data->capacity)

struct segment *segment_create(long size);
void segment_destroy(struct segment *seg);
void segment_reset(struct segment *seg);
long segment_count_active_objects(struct segment *seg);

#endif /* segment_h */