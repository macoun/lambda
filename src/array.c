#include "array.h"
#include <stdlib.h>
#include <stdio.h>

struct array *array_create(size_t initial_capacity)
{
    struct array *array = malloc(sizeof(struct array));
    if (!array)
        return NULL;

    array->cells = calloc(initial_capacity, sizeof(struct cell));
    if (!array->cells)
    {
        free(array);
        return NULL;
    }

    array->size = 0;
    array->capacity = initial_capacity;
    return array;
}

void array_destroy(struct array *array)
{
    if (array)
    {
        free(array->cells);
        free(array);
    }
}

void array_pushn(struct array *col, struct cell *first, long n)
{
    if (col->size + n > col->capacity)
    {
        fprintf(stderr, "array overflow %ld | %ld | %ld\n", col->size, col->capacity, n);
        exit(1);
    }
    for (long i = 0; i < n; i++)
    {
        col->cells[col->size++] = first[i];
    }
}

void array_push(struct array *array, struct cell value)
{
    if (array->size >= array->capacity)
    {
        fprintf(stderr, "array overflow\n");
        exit(1);
    }
    array->cells[array->size++] = value;
}

struct cell array_pop(struct array *array)
{
    if (array->size == 0)
    {
        fprintf(stderr, "Cannot pop on empty array\n");
        exit(1);
    }
    return array->cells[--array->size];
}

struct cell *array_peek(struct array *array, int depth)
{
    int idx = array->size - 1 - depth;
    if (idx < 0 || idx >= array->size)
    {
        return NULL;
    }
    return &array->cells[idx];
}

size_t array_size(const struct array *array)
{
    return array->size;
}
