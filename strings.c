#include "slimcc.h"

void *grow_array(void *data, size_t sz, int32_t *cap) {
  int64_t newcap = (int64_t)*cap * 2;
  if (newcap <= INT32_MAX) {
    if (newcap == 0)
      newcap = 8;
    void *p = realloc(data, sz * newcap);
    if (p) {
      *cap = newcap;
      return p;
    }
  }
  internal_error();
}

void strarray_push(StringArray *arr, const char *s) {
  if (arr->capacity == arr->len)
    GrowArr(arr->data, &arr->capacity);

  arr->data[arr->len++] = s;
}

// Takes a printf-style format string and returns a formatted string.
char *format(const char *fmt, ...) {
  char *buf;
  size_t buflen;
  FILE *out = open_memstream(&buf, &buflen);

  va_list ap;
  va_start(ap, fmt);
  vfprintf(out, fmt, ap);
  va_end(ap);
  fclose(out);
  return buf;
}
