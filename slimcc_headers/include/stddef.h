#ifndef __STDDEF_H
#define __STDDEF_H

#if __STDC_VERSION__ >= 202311L
#define __STDC_VERSION_STDDEF_H__ 202311L

#define unreachable() __builtin_unreachable()

typedef typeof(nullptr) nullptr_t;
#endif

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
