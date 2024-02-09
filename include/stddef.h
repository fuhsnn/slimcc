#ifndef __STDDEF_H
#define __STDDEF_H

#define NULL ((void *)0)

typedef unsigned long size_t;
typedef long ptrdiff_t;
typedef int wchar_t;
typedef struct {
  long long __ll;
  long double __ld;
} max_align_t;

#define offsetof(type, member) __builtin_offsetof(type, member)
#endif
