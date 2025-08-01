#ifndef __STDATOMIC_H
#define __STDATOMIC_H

#define ATOMIC_BOOL_LOCK_FREE 1
#define ATOMIC_CHAR_LOCK_FREE 1
#define ATOMIC_CHAR16_T_LOCK_FREE 1
#define ATOMIC_CHAR32_T_LOCK_FREE 1
#define ATOMIC_WCHAR_T_LOCK_FREE 1
#define ATOMIC_SHORT_LOCK_FREE 1
#define ATOMIC_INT_LOCK_FREE 1
#define ATOMIC_LONG_LOCK_FREE 1
#define ATOMIC_LLONG_LOCK_FREE 1
#define ATOMIC_POINTER_LOCK_FREE 1

typedef enum {
  memory_order_relaxed,
  memory_order_consume,
  memory_order_acquire,
  memory_order_release,
  memory_order_acq_rel,
  memory_order_seq_cst,
} memory_order;

#define ATOMIC_FLAG_INIT(x) (x)
#define atomic_init(addr, val) (*(addr) = (val))
#define kill_dependency(x) (x)
#define atomic_thread_fence(order) __builtin_atomic_thread_fence()
#define atomic_signal_fence(order)
#define atomic_is_lock_free(x) 1

#define atomic_load(addr) (*__builtin_atomic_chk(addr))
#define atomic_store(addr, val) (*__builtin_atomic_chk(addr) = (val))

#define atomic_load_explicit(addr, order) atomic_load(addr)
#define atomic_store_explicit(addr, val, order) atomic_store(addr, val)

#define atomic_fetch_add(addr, val) __builtin_atomic_fetch_add(addr, val)
#define atomic_fetch_sub(addr, val) __builtin_atomic_fetch_sub(addr, val)
#define atomic_fetch_or(addr, val) __builtin_atomic_fetch_or(addr, val)
#define atomic_fetch_xor(addr, val) __builtin_atomic_fetch_xor(addr, val)
#define atomic_fetch_and(addr, val) __builtin_atomic_fetch_and(addr, val)

#define atomic_fetch_add_explicit(addr, val, order) __builtin_atomic_fetch_add(addr, val)
#define atomic_fetch_sub_explicit(addr, val, order) __builtin_atomic_fetch_sub(addr, val)
#define atomic_fetch_or_explicit(addr, val, order) __builtin_atomic_fetch_or(addr, val)
#define atomic_fetch_xor_explicit(addr, val, order) __builtin_atomic_fetch_xor(addr, val)
#define atomic_fetch_and_explicit(addr, val, order) __builtin_atomic_fetch_and(addr, val)

#define atomic_compare_exchange_weak(p, old, new) \
  __builtin_compare_and_swap((p), (old), (new))

#define atomic_compare_exchange_strong(p, old, new) \
  __builtin_compare_and_swap((p), (old), (new))

#define atomic_compare_exchange_weak_explicit(p, old, new, succ, fail) \
  __builtin_compare_and_swap((p), (old), (new))

#define atomic_compare_exchange_strong_explicit(p, old, new, succ, fail) \
  __builtin_compare_and_swap((p), (old), (new))

#define atomic_exchange(obj, val) __builtin_atomic_exchange((obj), (val))
#define atomic_exchange_explicit(obj, val, order) __builtin_atomic_exchange((obj), (val))

#define atomic_flag_test_and_set(obj) atomic_exchange((obj), 1)
#define atomic_flag_test_and_set_explicit(obj, order) atomic_flag_test_and_set(obj)
#define atomic_flag_clear(obj) (*__builtin_atomic_chk(obj) = 0)
#define atomic_flag_clear_explicit(obj, order) atomic_flag_clear(obj)

typedef _Atomic _Bool atomic_flag;
typedef _Atomic _Bool atomic_bool;
typedef _Atomic char atomic_char;
typedef _Atomic signed char atomic_schar;
typedef _Atomic unsigned char atomic_uchar;
typedef _Atomic short atomic_short;
typedef _Atomic unsigned short atomic_ushort;
typedef _Atomic int atomic_int;
typedef _Atomic unsigned int atomic_uint;
typedef _Atomic long atomic_long;
typedef _Atomic unsigned long atomic_ulong;
typedef _Atomic long long atomic_llong;
typedef _Atomic unsigned long long atomic_ullong;
typedef _Atomic unsigned short atomic_char16_t;
typedef _Atomic unsigned atomic_char32_t;
typedef _Atomic unsigned atomic_wchar_t;
typedef _Atomic signed char atomic_int_least8_t;
typedef _Atomic unsigned char atomic_uint_least8_t;
typedef _Atomic short atomic_int_least16_t;
typedef _Atomic unsigned short atomic_uint_least16_t;
typedef _Atomic int atomic_int_least32_t;
typedef _Atomic unsigned int atomic_uint_least32_t;
typedef _Atomic long atomic_int_least64_t;
typedef _Atomic unsigned long atomic_uint_least64_t;
typedef _Atomic signed char atomic_int_fast8_t;
typedef _Atomic unsigned char atomic_uint_fast8_t;
typedef _Atomic short atomic_int_fast16_t;
typedef _Atomic unsigned short atomic_uint_fast16_t;
typedef _Atomic int atomic_int_fast32_t;
typedef _Atomic unsigned int atomic_uint_fast32_t;
typedef _Atomic long atomic_int_fast64_t;
typedef _Atomic unsigned long atomic_uint_fast64_t;
typedef _Atomic long atomic_intptr_t;
typedef _Atomic unsigned long atomic_uintptr_t;
typedef _Atomic unsigned long atomic_size_t;
typedef _Atomic long atomic_ptrdiff_t;
typedef _Atomic long atomic_intmax_t;
typedef _Atomic unsigned long atomic_uintmax_t;

#endif
