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

static bool use_arena;
static size_t page_use;
static Page *page;
static Page *head_page;

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

void arena_on(void) {
  if (!head_page)
    head_page = new_page();

  page = head_page;
  use_arena = true;
  page_use = 0;

#if USE_ASAN
  for (Page *p = head_page; p; p = p->next)
    __asan_poison_memory_region(&p->buf, PAGE_SIZE);
#endif
}

void arena_off(void) {
  use_arena = false;

#if USE_ASAN
  for (Page *p = head_page; p; p = p->next)
    __asan_poison_memory_region(&p->buf, PAGE_SIZE);
#endif
}

static void *allocate(size_t sz, bool clear) {
  size_t aligned_sz = (sz + 15) & -16LL;

  void *ptr;
  if ((page_use + aligned_sz) <= PAGE_SIZE) {
    ptr = &page->buf[page_use];
    page_use += aligned_sz;
  } else {
    if (!page->next)
      page->next = new_page();
    page = page->next;
    ptr = &page->buf;
    page_use = aligned_sz;
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
  if (!use_arena)
    return malloc(sz);
  return allocate(sz, false);
}

void *arena_calloc(size_t n, size_t sz) {
  if (!use_arena)
    return calloc(n, sz);
  return allocate(n * sz, true);
}
