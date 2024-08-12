#include "test.h"
#define STRINGIFY2(x) #x
#define STRINGIFY(x) STRINGIFY2(x)
#define ASSERTL(x, y) test_assert(x, y, #y " line:" STRINGIFY(__LINE__))

int main(int argc, char** argv) {

  void *lv1, *lv2;
  {
    int vla[argc], vla2[argc];
    lv1 = vla;
    lv2 = vla2;
  }
  {
    int vla[argc], vla2[argc];
    ASSERTL(1, lv1 == vla);
    ASSERTL(1, lv2 == vla2);
  }

  {
    int i = 0;
goto1:;
    int vla1[argc];
    int vla2[argc];
    ASSERTL(1, lv1 == vla1);
    ASSERTL(1, lv2 == vla2);
    if (i++ != argc)
      goto goto1;
  }

  {
    int i = 0;
    int vla1[argc];
    ASSERTL(1, lv1 == vla1);
    {
goto2:;
      int vla2[argc];
      ASSERTL(1, lv2 == vla2);
      if (i++ != argc)
        goto goto2;
      else
        goto goto3;
    }
goto3:;
    int vla2[argc];
    ASSERTL(1, lv2 == vla2);
  }

  switch(argc) {
    default:; {
        int vla1[argc];
        ASSERTL(1, lv1 == vla1);
      }
      int vla1[argc];
      ASSERTL(1, lv1 == vla1);
      {
        int vla2[argc];
        ASSERTL(1, lv2 == vla2);
      }
      int vla2[argc];
      ASSERTL(1, lv2 == vla2);
      break;
  }

int swctl = 0;
goto4:;
  switch(swctl) {
    case 0: {
      int vla1[argc];
      ASSERTL(1, lv1 == vla1);
    }
    case 1: {
      int vla1[argc];
      ASSERTL(1, lv1 == vla1);
      swctl++;
      switch (swctl) {
        case 1: {
          int vla2[argc];
          ASSERTL(1, lv2 == vla2);
        }
        case 2:;
          int vla2[argc];
          ASSERTL(1, lv2 == vla2);
          if(swctl == 1)
            goto goto4;
      }
      int vla2[argc];
      ASSERTL(1, lv2 == vla2);
      break;
    }
    default:;
      swctl++;
      int vla1[argc];
      int vla2[argc];
      ASSERTL(1, lv1 == vla1);
      ASSERTL(1, lv2 == vla2);
  }
  if (swctl == 2)
    goto goto4;

  switch(argc) {
    default: {
      int vla1[argc];
      int vla2[argc];
      ASSERTL(1, lv1 == vla1);
      ASSERTL(1, lv2 == vla2);
      break;
    }
  }


  int w0 = 0;
  while (w0++ != argc+1){
    int vla1[argc];
    int vla2[argc];
    ASSERTL(1, lv1 == vla1);
    ASSERTL(1, lv2 == vla2);
  }

  int d0 = 0;
  do {
    int vla1[argc];
    int vla2[argc];
    ASSERTL(1, lv1 == vla1);
    ASSERTL(1, lv2 == vla2);
  } while (d0++ != argc+1);

  int i0 = 0;
  for (int vla1[argc]; i0 != argc+1; i0++){
    ASSERTL(1, lv1 == vla1);
    int vla2[argc];
    ASSERTL(1, lv2 == vla2);
  }

  int i1 = 0;
  for (int vla1[argc];; i1++){
    ASSERTL(1, lv1 == vla1);
    int vla2[argc];
    ASSERTL(1, lv2 == vla2);
    if (i1 != argc+1)
      continue;
    else
      break;
  }

  for (int i = 0;; i++){
    int vla1[argc];
    ASSERTL(1, lv1 == vla1);
    switch (i) {
    case 0:
    case 2:
      continue;
    case 1:
    case 3:
      int vla2[argc];
      ASSERTL(1, lv2 == vla2);
      continue;
    }
    break;
  }

  ASSERTL(1, ({int vla1[argc]; vla1;}) == lv1);
  ASSERTL(1, ({int vla1[argc], vla2[argc]; vla2; }) == lv2);

  ASSERTL(1, ({int vla1[argc]; vla1;}) == ({int vla1[argc]; &vla1[0];}) );

  int vla1[argc], vla2[argc];
  ASSERTL(1, lv1 == vla1);
  ASSERTL(1, lv2 == vla2);

  printf("OK\n");
  return 0;
}
