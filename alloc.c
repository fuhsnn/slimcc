#include "slimcc.h"

#if USE_ASAN
#include <sanitizer/asan_interface.h>

const char *__asan_default_options(void) {
  return "detect_leaks=0";
}
#endif

#define FREE_THRESHOLD (100 * 1024)
#define PAGE_SIZE 4064

struct Page {
  char buf[PAGE_SIZE];
  Page *next;
};

Arena ast_arena;
Arena node_arena;
Arena pp_arena;
bool free_alloc;

bool check_mem_usage(void) {
#if USE_ASAN
  return true;
#else
  struct rusage stat;
  getrusage(RUSAGE_SELF, &stat);
  return stat.ru_maxrss > FREE_THRESHOLD;
#endif
}

static Page *new_page(void) {
  Page *p = malloc(sizeof(Page));
  p->next = NULL;
#if USE_ASAN
  __asan_poison_memory_region(&p->buf, PAGE_SIZE);
#endif
  return p;
}

static void *allocate(Arena *arena, size_t sz, bool clear) {
  size_t aligned_sz = (sz + 15) & -16LL;

  void *ptr;
  if ((arena->used + aligned_sz) <= PAGE_SIZE) {
    ptr = &arena->page->buf[arena->used];
    arena->used += aligned_sz;
  } else {
    if (!arena->page->next)
      arena->page->next = new_page();

    arena->page = arena->page->next;
    ptr = &arena->page->buf;
    arena->used = aligned_sz;
  }

#if USE_ASAN
  __asan_unpoison_memory_region(ptr, sz);
  if (clear)
    memset(ptr, 0, sz);
#else
  if (clear)
    for (int i = 0; i < (aligned_sz >> 3); i++)
      ((int64_t *)ptr)[i] = 0;
#endif
  return ptr;
}

void arena_on(Arena *arena) {
  if (!arena->head_page)
    arena->head_page = new_page();

  arena->page = arena->head_page;
  arena->on = true;
  arena->used = 0;
}

void arena_off(Arena *arena) {
  arena->on = false;

  if (free_alloc) {
    while (arena->head_page) {
      Page *nxt = arena->head_page->next;
      free(arena->head_page);
      arena->head_page = nxt;
    }
  }
}

void *arena_malloc(Arena *a, size_t sz) {
  return allocate(a, sz, false);
}

void *arena_calloc(Arena *a, size_t sz) {
  return allocate(a, sz, true);
}

void *ast_arena_malloc(size_t sz) {
  if (!ast_arena.on)
    return malloc(sz);
  return allocate(&ast_arena, sz, false);
}

void *ast_arena_calloc(size_t sz) {
  if (!ast_arena.on)
    return calloc(1, sz);
  return allocate(&ast_arena, sz, true);
}
