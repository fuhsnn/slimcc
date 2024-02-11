#include "test.h"

float g40 = 1.5;
double g41 = 0.0 ? 55 : (0, 1 + 1 * 5.0 / 2 * (double)2 * (int)2.0);

int main() {
  ASSERT(10, ({ enum { ten=1+2+3+4 }; ten; }));
  ASSERT(1, ({ int i=0; switch(3) { case 5-2+0*3: i++; } i; }));
  EASSERT(8, sizeof(int[1+1]));
  EASSERT(6, 8-2);
  EASSERT(6, 2*3);
  EASSERT(3, 12/4);
  EASSERT(2, 12%10);
  EASSERT(0b100, 0b110&0b101);
  EASSERT(0b111, 0b110|0b101);
  EASSERT(0b110, 0b111^0b001);
  EASSERT(4, 1<<2);
  EASSERT(2, 4>>1);
  EASSERT(2, (1==1)+1);
  EASSERT(1, (1!=1)+1);
  EASSERT(1, (1<1)+1);
  EASSERT(2, (1<=1)+1);
  EASSERT(2, 1?2:3);
  EASSERT(3, 0?2:3);
  EASSERT(3, (1,3));
  EASSERT(2, !0+1);
  EASSERT(1, !1+1);
  EASSERT(2, ~-3);
  EASSERT(2, (5||6)+1);
  EASSERT(1, (0||0)+1);
  EASSERT(2, (1&&1)+1);
  EASSERT(1, (1&&0)+1);
  EASSERT(3, (int)3);
  EASSERT(15, (char)0xffffff0f);
  EASSERT(0x10f, (short)0xffff010f);
  EASSERT(4, (int)0xfffffffffff+5);
  EASSERT(8, (int*)0+2);
  EASSERT(12, (int*)16-1);
  EASSERT(3, (int*)16-(int*)4);

  EASSERT(4, (-1>>31)+5);
  EASSERT(255, (unsigned char)0xffffffff);
  EASSERT(0x800f, (unsigned short)0xffff800f);
  EASSERT(1, (unsigned int)0xfffffffffff>>31);
  EASSERT(1, (long)-1/((long)1<<62)+1);
  EASSERT(4, (unsigned long)-1/((long)1<<62)+1);
  EASSERT(1, (unsigned)1<-1);
  EASSERT(1, (unsigned)1<=-1);

  ASSERT(1, g40==1.5);
  ASSERT(1, g41==11);

  printf("OK\n");
  return 0;
}
