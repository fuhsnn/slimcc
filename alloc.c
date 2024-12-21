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

#define FREE_THRESHOLD (100 * 1024)

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
