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
