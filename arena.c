#include "slimcc.h"

#if defined(__SANITIZE_ADDRESS__)
#define USE_ASAN 1
#elif defined(__has_feature)
#if __has_feature(address_sanitizer)
#define USE_ASAN 1
#endif
#endif

#if USE_ASAN
#include <sanitizer/asan_interface.h>

const char *__asan_default_options(void) {
  return "detect_leaks=0";
}
#endif

#define PAGE_SIZE 4048

typedef struct Page Page;
struct Page {
#if USE_ASAN
  Page *next;
  __attribute__((aligned(16))) char buf[PAGE_SIZE];
#else
  char buf[PAGE_SIZE];
  Page *next;
#endif
};

typedef struct {
  bool on;
  size_t used;
  Page *page;
  Page *head_page;
} Arena;

static Arena ast_arena;
static Arena pp_arena;

static Page *new_page(void) {
  Page *p;
  if (posix_memalign((void **)&p, 16, sizeof(Page)))
    error("posix_memalign failed");
  p->next = NULL;

#if USE_ASAN
  __asan_poison_memory_region(&p->buf, PAGE_SIZE);
#endif
  return p;
}

static void arena_on2(Arena *arena) {
  if (!arena->head_page)
    arena->head_page = new_page();

  arena->page = arena->head_page;
  arena->on = true;
  arena->used = 0;
}


static void arena_off2(Arena *arena) {
  arena->on = false;

  while (arena->head_page) {
    Page *nxt = arena->head_page->next;
    free(arena->head_page);
    arena->head_page = nxt;
  }
}

void arena_combine(void) {
  // if (ast_arena.page) {
  //   while (ast_arena.page->next)
  //     ast_arena.page = ast_arena.page->next;
  //   ast_arena.page->next = pp_arena.head_page;
  // } else {
  //   ast_arena.head_page = pp_arena.head_page;
  // }
  // pp_arena_off();
  // pp_arena.head_page = NULL;
}

void arena_on(void) {
  arena_on2(&ast_arena);
}

void arena_off(void) {
  arena_off2(&ast_arena);
}

void pp_arena_on(void) {
  arena_on2(&pp_arena);
}

void pp_arena_off(void) {
  arena_off2(&pp_arena);
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
    memset(ptr, 0, aligned_sz);
#endif
  return ptr;
}

void *arena_malloc(size_t sz) {
  if (!ast_arena.on)
    return malloc(sz);
  return allocate(&ast_arena, sz, false);
}

void *arena_calloc(size_t n, size_t sz) {
  if (!ast_arena.on)
    return calloc(n, sz);
  return allocate(&ast_arena, n * sz, true);
}

void *pp_arena_malloc(size_t sz) {
  if (!pp_arena.on)
    return malloc(sz);
  return allocate(&pp_arena, sz, false);
}

void *pp_arena_calloc(size_t n, size_t sz) {
  if (!pp_arena.on)
    return calloc(n, sz);
  return allocate(&pp_arena, n * sz, true);
}
