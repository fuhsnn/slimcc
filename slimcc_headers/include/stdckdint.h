#ifndef __STDC_VERSION_STDCKDINT_H__
#define __STDC_VERSION_STDCKDINT_H__ 202311L

#define ckd_add(_ptr, _lh, _rh) __builtin_add_overflow((_lh), (_rh), (_ptr))
#define ckd_sub(_ptr, _lh, _rh) __builtin_sub_overflow((_lh), (_rh), (_ptr))
#define ckd_mul(_ptr, _lh, _rh) __builtin_mul_overflow((_lh), (_rh), (_ptr))

#endif
