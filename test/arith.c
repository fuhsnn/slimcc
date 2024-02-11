#include "test.h"

int main() {
  EASSERT(0, 0);
  EASSERT(42, 42);
  EASSERT(21, 5+20-4);
  EASSERT(41,  12 + 34 - 5 );
  EASSERT(47, 5+6*7);
  EASSERT(15, 5*(9-6));
  EASSERT(4, (3+5)/2);
  EASSERT(10, -10+20);
  EASSERT(10, - -10);
  EASSERT(10, - - +10);

  EASSERT(0, 0==1);
  EASSERT(1, 42==42);
  EASSERT(1, 0!=1);
  EASSERT(0, 42!=42);

  EASSERT(1, 0<1);
  EASSERT(0, 1<1);
  EASSERT(0, 2<1);
  EASSERT(1, 0<=1);
  EASSERT(1, 1<=1);
  EASSERT(0, 2<=1);

  EASSERT(1, 1>0);
  EASSERT(0, 1>1);
  EASSERT(0, 1>2);
  EASSERT(1, 1>=0);
  EASSERT(1, 1>=1);
  EASSERT(0, 1>=2);

  EASSERT(0, 1073741824 * 100 / 100);

  ASSERT(7, ({ int i=2; i+=5; i; }));
  ASSERT(7, ({ int i=2; i+=5; }));
  ASSERT(3, ({ int i=5; i-=2; i; }));
  ASSERT(3, ({ int i=5; i-=2; }));
  ASSERT(6, ({ int i=3; i*=2; i; }));
  ASSERT(6, ({ int i=3; i*=2; }));
  ASSERT(3, ({ int i=6; i/=2; i; }));
  ASSERT(3, ({ int i=6; i/=2; }));

  ASSERT(3, ({ int i=2; ++i; }));
  ASSERT(2, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; ++*p; }));
  ASSERT(0, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; --*p; }));

  ASSERT(2, ({ int i=2; i++; }));
  ASSERT(2, ({ int i=2; i--; }));
  ASSERT(3, ({ int i=2; i++; i; }));
  ASSERT(1, ({ int i=2; i--; i; }));
  ASSERT(1, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; *p++; }));
  ASSERT(1, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; *p--; }));

  ASSERT(0, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; (*p++)--; a[0]; }));
  ASSERT(0, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; (*(p--))--; a[1]; }));
  ASSERT(2, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; (*p)--; a[2]; }));
  ASSERT(2, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; (*p)--; p++; *p; }));

  ASSERT(0, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; (*p++)--; a[0]; }));
  ASSERT(0, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; (*p++)--; a[1]; }));
  ASSERT(2, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; (*p++)--; a[2]; }));
  ASSERT(2, ({ int a[3]; a[0]=0; a[1]=1; a[2]=2; int *p=a+1; (*p++)--; *p; }));

  EASSERT(0, !1);
  EASSERT(0, !2);
  EASSERT(1, !0);
  EASSERT(1, !(char)0);
  EASSERT(0, !(long)3);
  EASSERT(4, sizeof(!(char)0));
  EASSERT(4, sizeof(!(long)0));

  EASSERT(-1, ~0);
  EASSERT(0, ~-1);

  EASSERT(5, 17%6);
  EASSERT(5, ((long)17)%6);
  ASSERT(2, ({ int i=10; i%=4; i; }));
  ASSERT(2, ({ long i=10; i%=4; i; }));

  EASSERT(0, 0&1);
  EASSERT(1, 3&1);
  EASSERT(3, 7&3);
  EASSERT(10, -1&10);

  EASSERT(1, 0|1);
  EASSERT(0b10011, 0b10000|0b00011);

  EASSERT(0, 0^0);
  EASSERT(0, 0b1111^0b1111);
  EASSERT(0b110100, 0b111000^0b001100);

  ASSERT(2, ({ int i=6; i&=3; i; }));
  ASSERT(7, ({ int i=6; i|=3; i; }));
  ASSERT(10, ({ int i=15; i^=5; i; }));

  EASSERT(1, 1<<0);
  EASSERT(8, 1<<3);
  EASSERT(10, 5<<1);
  EASSERT(2, 5>>1);
  EASSERT(-1, -1>>1);
  ASSERT(1, ({ int i=1; i<<=0; i; }));
  ASSERT(8, ({ int i=1; i<<=3; i; }));
  ASSERT(10, ({ int i=5; i<<=1; i; }));
  ASSERT(2, ({ int i=5; i>>=1; i; }));
  EASSERT(-1, -1);
  ASSERT(-1, ({ int i=-1; i; }));
  ASSERT(-1, ({ int i=-1; i>>=1; i; }));

  EASSERT(2, 0?1:2);
  EASSERT(1, 1?1:2);
  EASSERT(-1, 0?-2:-1);
  EASSERT(-2, 1?-2:-1);
  EASSERT(4, sizeof(0?1:2));
  EASSERT(8, sizeof(0?(long)1:(long)2));
  EASSERT(-1, 0?(long)-2:-1);
  EASSERT(-1, 0?-2:(long)-1);
  EASSERT(-2, 1?(long)-2:-1);
  EASSERT(-2, 1?-2:(long)-1);

  1 ? -2 : (void)-1;

  ASSERT(20, ({ int x; int *p=&x; p+20-p; }));
  ASSERT(1, ({ int x; int *p=&x; p+20-p>0; }));
  ASSERT(-20, ({ int x; int *p=&x; p-20-p; }));
  ASSERT(1, ({ int x; int *p=&x; p-20-p<0; }));

  EASSERT(15, (char *)0xffffffffffffffff - (char *)0xfffffffffffffff0);
  EASSERT(-15, (char *)0xfffffffffffffff0 - (char *)0xffffffffffffffff);
  EASSERT(1, (void *)0xffffffffffffffff > (void *)0);

  ASSERT(3, 3?:5);
  ASSERT(5, 0?:5);
  ASSERT(4, ({ int i = 3; ++i?:10; }));

  EASSERT(3, (long double)3);
  EASSERT(5, (long double)3+2);
  EASSERT(6, (long double)3*2);
  EASSERT(5, (long double)3+2.0);

  printf("OK\n");
  return 0;
}
