//
//  mem.c
//  Lisper - Memory Management System
//
//  Created by Ferhat Ayaz on 13/03/2016.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#include "mem.h"
#include "logger.h"
#include "array.h"
#include "gc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <stdbool.h>

/*-----------------------------------------------------------------------*/
/* Constants and Types                                                   */
/*-----------------------------------------------------------------------*/

const struct cell NIL = mk_cell(0, NULL);
static const bool _false = false;
const struct cell FALSE = mk_cell(CUSTOM, &_false);
static const bool _true = true;
const struct cell TRUE = mk_cell(CUSTOM, &_true);

// Configuration constants
#define DEFAULT_SEGMENT_SIZE 108 * 5
#define SEGMENT_MAX_IDLE_COUNT 2

/*-----------------------------------------------------------------------*/
/* Private Function Declarations                                         */
/*-----------------------------------------------------------------------*/

static struct segment *mem_get_free_segment(struct memory *mem);
static struct segment *mem_create_segment(struct memory *mem, long size, enum memspace space);
static void mem_reclaim_idle_segments(struct memory *mem);
static struct cell *mem_alloc_from_active(struct memory *mem, long size);
static struct cell *mem_alloc_from_inactive(struct memory *mem, long size);
static inline bool segment_has_space(struct segment *seg, long size);
static void mem_run_gc(struct memory *mem);
static void mem_record_gc_stats(struct memory *mem, long objects_before,
                                long objects_after, clock_t duration_ms);
/*-----------------------------------------------------------------------*/
/* Segment Management Functions                                          */
/*-----------------------------------------------------------------------*/

/**
 * Check if a segment has enough space for an allocation
 */
static inline bool segment_has_space(struct segment *seg, long size)
{
  return seg && (seg->data->size + size <= seg->data->capacity);
}

/**
 * Gets a recycled segment from the free segment pool
 */
static struct segment *mem_get_free_segment(struct memory *mem)
{
  if (!mem->freesegs)
  {
    return NULL;
  }

  struct segment *seg = mem->freesegs;
  mem->freesegs = mem->freesegs->next;
  segment_reset(seg);

  return seg;
}

/**
 * Creates a new segment or recycles an existing one from the pool
 * Returns NULL on allocation failure.
 */
static struct segment *mem_create_segment(struct memory *mem, long size, enum memspace space)
{
  struct segment *seg = NULL;

  // Try to reuse a segment from the pool first
  seg = mem_get_free_segment(mem);

  // Create new segment if no recyclable segment available
  if (!seg)
  {
    seg = segment_create(size);
    if (!seg)
    {
      error("Memory allocation failed for segment of size %ld", size);
      return NULL;
    }

    mem->created_segment_count++;
    debug("Creating new segment of size %ld [total: %ld]",
          size, mem->created_segment_count);
  }

  // Add to the appropriate space
  switch (space)
  {
  case MEMSPACE_ACTIVE:
    // Add to active list (front)
    seg->next = mem->segments;
    mem->segments = seg;
    break;

  case MEMSPACE_INACTIVE:
    // Add to inactive list (end for better locality)
    seg->next = NULL;
    if (!mem->inactive)
    {
      mem->inactive = seg;
    }
    else
    {
      // Find tail
      struct segment *tail = mem->inactive;
      while (tail->next)
      {
        tail = tail->next;
      }
      tail->next = seg;
    }
    break;

  case MEMSPACE_ROOTS:
    // Add to roots list (front)
    seg->next = mem->roots;
    mem->roots = seg;
    break;

  default:
    error("Unknown memory space %d", space);
    // Free segment if we created a new one
    if (seg->recycle_count == 0)
    {
      segment_destroy(seg);
    }
    return NULL;
  }

  return seg;
}

/**
 * Reclaim segments that have been idle for too long
 */
static void mem_reclaim_idle_segments(struct memory *mem)
{
  if (!mem->freesegs)
  {
    return; // No segments to reclaim
  }

  struct segment *seg = mem->freesegs;
  struct segment *prev = NULL;
  struct segment *next;

  while (seg)
  {
    if (seg->idle_count > SEGMENT_MAX_IDLE_COUNT)
    {
      // Remove this segment from the freelist
      next = seg->next;

      if (prev)
      {
        prev->next = next;
      }
      else
      {
        mem->freesegs = next;
      }

      debug("Reclaiming segment %p (recycle count %d)",
            (void *)seg, seg->recycle_count);

      segment_destroy(seg);
      seg = next;
    }
    else
    {
      // Keep this segment, increment idle count
      seg->idle_count++;
      prev = seg;
      seg = seg->next;
    }
  }
}

/*-----------------------------------------------------------------------*/
/* Memory Lifecycle Functions                                            */
/*-----------------------------------------------------------------------*/

/**
 * Create and initialize the memory management system
 */
struct memory *memory_create(void)
{
  // Allocate the memory manager
  struct memory *mem = calloc(1, sizeof(struct memory));
  if (!mem)
  {
    mem->gc_enabled = true;
    error("Failed to allocate memory manager");
    return NULL;
  }

  info("Creating memory management system");

  // Create initial active segment
  mem->segments = mem_create_segment(mem, DEFAULT_SEGMENT_SIZE, MEMSPACE_ACTIVE);
  if (!mem->segments)
  {
    error("Failed to create initial segment");
    free(mem);
    return NULL;
  }

  // Initialize GC statistics
  mem->gc_stats = calloc(1, sizeof(struct gc_stats));
  if (!mem->gc_stats)
  {
    error("Failed to allocate GC statistics");
    segment_destroy(mem->segments);
    free(mem);
    return NULL;
  }

  info("Memory manager initialized with segment size %ld",
       (long)DEFAULT_SEGMENT_SIZE);

  return mem;
}

/**
 * Free all memory used by the memory management system
 */
void memory_destroy(struct memory *mem)
{
  if (!mem)
  {
    return;
  }

  info("Destroying memory management system");

  // Free all segment lists
  segment_destroy_all(mem->segments);
  segment_destroy_all(mem->freesegs);
  segment_destroy_all(mem->inactive);
  segment_destroy_all(mem->roots);

  // Free statistics
  if (mem->gc_stats)
  {
    free(mem->gc_stats);
  }

  // Free the memory manager itself
  free(mem);
}

/*-----------------------------------------------------------------------*/
/* Memory Allocation Functions                                           */
/*-----------------------------------------------------------------------*/

/**
 * Allocate memory for a root array
 */
struct array *memory_alloc_root(struct memory *mem, long size)
{
  if (!mem || size <= 0)
  {
    error("Invalid parameters to memory_alloc_root");
    return NULL;
  }

  struct segment *seg = mem_create_segment(mem, size, MEMSPACE_ROOTS);
  if (!seg)
  {
    error("Failed to allocate root array of size %ld", size);
    return NULL;
  }

  return seg->data;
}

/**
 * Allocate cells from active memory space
 */
static struct cell *mem_alloc_from_active(struct memory *mem, long size)
{
  assert(mem && size > 0);

  // Check if there's enough space in current segment
  if (!segment_has_space(mem->segments, size))
  {
    // Try garbage collection first
    mem_run_gc(mem);

    // Check if GC freed enough space
    if (!segment_has_space(mem->segments, size))
    {
      // Create a new segment if needed
      struct segment *new_seg = mem_create_segment(
          mem,
          size > DEFAULT_SEGMENT_SIZE ? size : DEFAULT_SEGMENT_SIZE,
          MEMSPACE_ACTIVE);

      if (!new_seg)
      {
        error("Out of memory in active space allocation");
        return NULL;
      }
    }
  }

  // Allocate from the current segment
  struct segment *seg = mem->segments;
  struct array *data = seg->data;

  long index = data->size;
  data->size += size;

  return data->cells + index;
}

/**
 * Allocate cells from inactive memory space (used during GC)
 */
static struct cell *mem_alloc_from_inactive(struct memory *mem, long size)
{
  assert(mem && size > 0);

  // Find a segment with enough space, or create one
  struct segment *seg = mem->inactive;

  // Find last segment and check if it has space
  while (seg)
  {
    if (segment_has_space(seg, size))
    {
      break;
    }
    seg = seg->next;
  }

  // If no segment has space, create a new one
  if (!seg)
  {
    seg = mem_create_segment(
        mem,
        size > DEFAULT_SEGMENT_SIZE ? size : DEFAULT_SEGMENT_SIZE,
        MEMSPACE_INACTIVE);

    if (!seg)
    {
      error("Out of memory in inactive space allocation");
      return NULL;
    }
  }

  // Allocate from the segment
  struct array *data = seg->data;
  long index = data->size;
  data->size += size;

  return data->cells + index;
}

/**
 * Public allocation function - delegates to appropriate space allocator
 */
struct cell *memory_alloc(struct memory *mem, long size, bool active)
{
  if (!mem || size <= 0)
  {
    error("Invalid parameters to memory_alloc");
    return NULL;
  }

  if (active)
  {
    return mem_alloc_from_active(mem, size);
  }
  else
  {
    return mem_alloc_from_inactive(mem, size);
  }
}

/*-----------------------------------------------------------------------*/
/* Memory Utility Functions                                              */
/*-----------------------------------------------------------------------*/

/**
 * Scan memory segments and apply callback to each segment's data
 */
void memory_scan(struct memory *mem, memory_callback_f callback, enum memspace space)
{
  if (!mem || !callback)
  {
    return;
  }

  struct segment *curroot = NULL;

  // Select segment list based on space
  switch (space)
  {
  case MEMSPACE_ACTIVE:
    curroot = mem->segments;
    break;
  case MEMSPACE_INACTIVE:
    curroot = mem->inactive;
    break;
  case MEMSPACE_ROOTS:
    curroot = mem->roots;
    break;
  default:
    error("Invalid memory space in scan: %d", space);
    return;
  }

  // Apply callback to each segment
  while (curroot)
  {
    if (!callback(mem, curroot->data))
    {
      break; // Stop if callback returns false
    }
    curroot = curroot->next;
  }
}

/**
 * Flip memory spaces after garbage collection
 */
void memory_flip(struct memory *mem)
{
  if (!mem)
  {
    return;
  }

  // Reclaim segments that have been idle too long
  mem_reclaim_idle_segments(mem);

  // If there are no active segments, something is wrong
  if (!mem->segments)
  {
    error("No active segments during memory flip");
    return;
  }

  // Move active segments to free list
  struct segment *last_active = mem->segments;
  while (last_active->next)
  {
    last_active = last_active->next;
  }

  last_active->next = mem->freesegs;
  mem->freesegs = mem->segments;

  // Make inactive segments active
  mem->segments = mem->inactive;
  mem->inactive = NULL;
}

/**
 * Count active objects in memory
 */
long memory_active_object_count(struct memory *mem)
{
  return mem ? segment_count_active_objects(mem->segments) : 0;
}

void memory_enable_gc(struct memory *mem, bool enable)
{
  if (!mem)
  {
    return;
  }
  mem->gc_enabled = enable;
  if (enable)
  {
    info("Garbage collection enabled");
    gc(mem);
  }
  else
  {
    info("Garbage collection disabled");
  }
}

// Get current time in milliseconds
static inline clock_t get_time_ms()
{
  return clock() * 1000 / CLOCKS_PER_SEC;
}

static void mem_run_gc(struct memory *mem)
{
  long before, after;
  clock_t start_time, end_time;
  clock_t duration;

  if (!mem || !mem->gc_enabled)
  {
    info("Garbage collection disabled. Skipping GC.");
    return;
  }

  // Record pre-GC metrics
  before = memory_active_object_count(mem);
  start_time = get_time_ms();

  // debug("Starting garbage collection...");
  gc(mem);

  // Calculate GC duration and results
  end_time = get_time_ms();
  duration = end_time - start_time;
  after = memory_active_object_count(mem);

  // Record statistics in the stats module
  mem_record_gc_stats(mem, before, after, duration);

  // info("GC: Collected %ld objects in %ld ms", before - after, duration);
}

/**
 * Record GC collection statistics
 */
static void mem_record_gc_stats(struct memory *mem, long objects_before,
                                long objects_after, clock_t duration_ms)
{
  if (!mem || !mem->gc_stats)
  {
    return;
  }

  struct gc_stats *stats = mem->gc_stats;
  stats->collections++;

  long objects_collected = objects_before - objects_after;
  stats->objects_collected += objects_collected;
  stats->last_gc_time_ms = duration_ms;
  stats->total_gc_time_ms += duration_ms;

  // Calculate collection rate (percentage collected)
  double rate = objects_before > 0
                    ? (double)objects_collected * 100.0 / objects_before
                    : 0.0;

  // Update running average
  stats->avg_collection_rate =
      (stats->avg_collection_rate * (stats->collections - 1) + rate) /
      stats->collections;
}

/**
 * Print garbage collection statistics to stdout
 */
void memory_print_stats(struct memory *mem)
{
  if (!mem || !mem->gc_stats)
  {
    printf("No statistics available\n");
    return;
  }

  struct gc_stats *stats = mem->gc_stats;

  printf("\n===== Memory Management Statistics =====\n");

  // Basic statistics
  printf("GC Collections:     %ld\n", stats->collections);
  printf("Objects collected:  %ld\n", stats->objects_collected);
  printf("Collection rate:    %.2f%%\n", stats->avg_collection_rate);
  printf("Total GC time:      %.2f ms\n", (double)stats->total_gc_time_ms);

  // Memory usage
  long live_objects = memory_active_object_count(mem);
  long total_segs = mem->created_segment_count;
  printf("Live objects:       %ld\n", live_objects);
  printf("Total segments:     %ld\n", total_segs);

  printf("\n--- Detailed Statistics ---\n");

  // Segment counts by type
  long active_segs = segment_count_segments(mem->segments);
  long inactive_segs = segment_count_segments(mem->inactive);
  long free_segs = segment_count_segments(mem->freesegs);
  long root_segs = segment_count_segments(mem->roots);

  printf("Active segments:    %ld\n", active_segs);
  printf("Inactive segments:  %ld\n", inactive_segs);
  printf("Free segments:      %ld\n", free_segs);
  printf("Root segments:      %ld\n", root_segs);

  // GC timing details
  printf("Last GC duration:   %ld ms\n", stats->last_gc_time_ms);
  double avg_gc_time = stats->collections > 0 ? (double)stats->total_gc_time_ms / stats->collections : 0;
  printf("Average GC time:    %.2f ms\n", avg_gc_time);

  // Memory efficiency
  if (stats->collections > 0)
  {
    double avg_collected_per_gc = (double)stats->objects_collected / stats->collections;
    printf("Avg objects per GC: %.2f\n", avg_collected_per_gc);
  }

  printf("=======================================\n\n");
}