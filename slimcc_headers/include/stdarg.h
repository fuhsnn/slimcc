#ifndef __STDARG_H
#define __STDARG_H

typedef __builtin_va_list va_list;

#define va_start(ap, ...) __builtin_va_start(ap)

#define va_end(ap) __builtin_va_end(ap)

#define va_arg(ap, ty) __builtin_va_arg(ap, ty)

#define va_copy(dest, src) __builtin_va_copy(dest, src)

#define __GNUC_VA_LIST 1
typedef va_list __gnuc_va_list;

#endif
