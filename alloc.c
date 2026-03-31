#include "slimcc.h"

#if USE_ASAN
# include <sanitizer/asan_interface.h>

__attribute__((visibility("default"))) const char *__asan_default_options(void) {
  return "detect_leaks=0";
}
#endif

#define FREE_THRESHOLD (100 * 1024)
#define ARENA_POOL_SIZE 8168

struct Pool {
  char buf[ARENA_POOL_SIZE];
  Pool *next;
};

Arena ast_arena;
Arena node_arena;
Arena pp_arena;
bool free_alloc;

static Pool *pool_freelist;

bool check_mem_usage(void) {
#if USE_ASAN || defined(__FILC__)
  return true;
#else
  struct rusage stat;
  getrusage(RUSAGE_SELF, &stat);
  return stat.ru_maxrss > FREE_THRESHOLD;
#endif
}

static Pool *new_pool(void) {
  Pool *p;
  if (pool_freelist) {
    p = pool_freelist;
    pool_freelist = p->next;
  } else {
    p = malloc(sizeof(Pool));
  }

#if USE_ASAN
  __asan_poison_memory_region(&p->buf, ARENA_POOL_SIZE);
#endif
  p->next = NULL;
  return p;
}

static void *allocate(Arena *arena, size_t sz, bool clear) {
  size_t aligned_sz = (sz + 15) & -16LL;

  void *ptr;
  if ((arena->used + aligned_sz) <= ARENA_POOL_SIZE) {
    ptr = &arena->cur->buf[arena->used];
    arena->used += aligned_sz;
  } else {
    arena->cur = arena->cur->next = new_pool();
    ptr = &arena->cur->buf;
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
  arena->head = arena->cur = new_pool();
  arena->used = 0;
}

void arena_off(Arena *arena) {
  if (free_alloc) {
    for (Pool *p = arena->head; p;) {
      Pool *tmp = p;
      p = p->next;
      free(tmp);
    }
  } else {
    arena->cur->next = pool_freelist;
    pool_freelist = arena->head;
  }
  arena->cur = NULL;
}

void *arena_malloc(Arena *a, size_t sz) {
  return allocate(a, sz, false);
}

void *arena_calloc(Arena *a, size_t sz) {
  return allocate(a, sz, true);
}

void *ast_arena_malloc(size_t sz) {
  if (!ast_arena.cur)
    return malloc(sz);
  return allocate(&ast_arena, sz, false);
}

void *ast_arena_calloc(size_t sz) {
  if (!ast_arena.cur)
    return calloc(1, sz);
  return allocate(&ast_arena, sz, true);
}
