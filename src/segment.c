#include "segment.h"

#include <stdio.h>
#include <stdlib.h>

#define segment_full(s) (array_full((s)->data))
#define segment_can_add(s, n) ((s)->data->size + (n) < (s)->data->capacity)

struct segment *segment_create(long size)
{
    struct segment *seg = NULL;
    seg = (struct segment *)malloc(sizeof(struct segment));
    if (!seg)
    {
        fprintf(stderr, "Memory allocation failed for segment\n");
        exit(1);
    }

    seg->data = array_create(size);
    if (!seg->data)
    {
        fprintf(stderr, "Memory allocation failed for segment cells\n");
        free(seg);
        exit(1);
    }
    seg->idle_count = 0;
    seg->recycle_count = 0;

    return seg;
}

void segment_destroy_all(struct segment *seg)
{
    struct segment *next;
    while (seg)
    {
        next = seg->next;
        segment_destroy(seg);
        seg = next;
    }
}

void segment_destroy(struct segment *seg)
{
    if (seg)
    {
        fprintf(stdout, "Destroying segment\n");
        array_destroy(seg->data);
        free(seg);
    }
}

long segment_count_active_objects(struct segment *seg)
{
    long total = 0;
    while (seg)
    {
        total += seg->data->size;
        seg = seg->next;
    }
    return total;
}

void segment_reset(struct segment *seg)
{
    if (seg)
    {
        seg->data->size = 0;
        seg->idle_count = 0;
        seg->recycle_count++;
    }
}
/**
 * Count the number of segments in a chain
 */
long segment_count_segments(struct segment *seg)
{
    long count = 0;
    while (seg)
    {
        count++;
        seg = seg->next;
    }
    return count;
}