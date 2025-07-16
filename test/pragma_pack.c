#include "test.h"

int main(void) {

#define CHK(_ALIGN) \
  { struct S { char c; float f; }; SASSERT((_ALIGN + 4) == sizeof(struct S)); SASSERT((_ALIGN < 4 ? _ALIGN : 4) == _Alignof(struct S)); }

#pragma pack(push)
  CHK(4)
#pragma pack(pop)
  CHK(4)
#pragma pack(2)
  CHK(2)
#define PACK(_x) _Pragma("pack(push)") _Pragma("pack(1)") _x _Pragma("pack (pop)")
  {
    PACK(
      struct S {
        char c;
        float f;
      };
    )
     SASSERT(5 == sizeof(struct S));
     SASSERT(1 == _Alignof(struct S));
  }
  CHK(2)
#pragma pack(push)
  CHK(2)
#pragma pack()
  CHK(4)
#pragma pack(1)
  CHK(1)
#pragma pack(8)
  CHK(4)
#pragma pack(pop)
  CHK(2)
#pragma pack(push, 1)
  CHK(1)
#pragma pack(pop)
  CHK(2)

#pragma pack(0)
 { struct S { char a : 5, b : 5, c : 5; }; SASSERT(sizeof(struct S) == 3); SASSERT(_Alignof(struct S) == 1); }
#pragma pack(1)
 { struct S { char a : 5, b : 5, c : 5; }; SASSERT(sizeof(struct S) == 2); SASSERT(_Alignof(struct S) == 1); }
#pragma pack(2)
 { struct S { char a : 5, b : 5, c : 5; }; SASSERT(sizeof(struct S) == 2); SASSERT(_Alignof(struct S) == 1); }

#pragma pack(0)
 { struct S { short a : 10, b : 10, c : 10; }; SASSERT(sizeof(struct S) == 6); SASSERT(_Alignof(struct S) == 2); }
#pragma pack(1)
 { struct S { short a : 10, b : 10, c : 10; }; SASSERT(sizeof(struct S) == 4); SASSERT(_Alignof(struct S) == 1); }
#pragma pack(2)
 { struct S { short a : 10, b : 10, c : 10; }; SASSERT(sizeof(struct S) == 4); SASSERT(_Alignof(struct S) == 2); }
#pragma pack(4)
 { struct S { short a : 10, b : 10, c : 10; }; SASSERT(sizeof(struct S) == 4); SASSERT(_Alignof(struct S) == 2); }

#pragma pack(0)
 { struct S { int a : 21, b : 21, c : 21; }; SASSERT(sizeof(struct S) == 12); SASSERT(_Alignof(struct S) == 4); }
#pragma pack(1)
 { struct S { int a : 21, b : 21, c : 21; }; SASSERT(sizeof(struct S) == 8); SASSERT(_Alignof(struct S) == 1); }
#pragma pack(2)
 { struct S { int a : 21, b : 21, c : 21; }; SASSERT(sizeof(struct S) == 8); SASSERT(_Alignof(struct S) == 2); }
#pragma pack(4)
 { struct S { int a : 21, b : 21, c : 21; }; SASSERT(sizeof(struct S) == 8); SASSERT(_Alignof(struct S) == 4); }
#pragma pack(8)
 { struct S { int a : 21, b : 21, c : 21; }; SASSERT(sizeof(struct S) == 8); SASSERT(_Alignof(struct S) == 4); }

#if defined(__slimcc__) || defined(_MSC_VER)
#pragma pack(1)
  struct S {
    char a;
    _Alignas(1024) char b;
  };
  static_assert(2048 == sizeof(struct S));
#endif

  printf("OK\n");
}

