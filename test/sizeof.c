#include "test.h"

int main() {
  EASSERT(1, sizeof(char));
  EASSERT(2, sizeof(short));
  EASSERT(2, sizeof(short int));
  EASSERT(2, sizeof(int short));
  EASSERT(4, sizeof(int));
  EASSERT(8, sizeof(long));
  EASSERT(8, sizeof(long int));
  EASSERT(8, sizeof(long int));
  EASSERT(8, sizeof(char *));
  EASSERT(8, sizeof(int *));
  EASSERT(8, sizeof(long *));
  EASSERT(8, sizeof(int **));
  EASSERT(8, sizeof(int(*)[4]));
  EASSERT(32, sizeof(int*[4]));
  EASSERT(16, sizeof(int[4]));
  EASSERT(48, sizeof(int[3][4]));
  EASSERT(8, sizeof(struct {int a; int b;}));

  EASSERT(8, sizeof(-10 + (long)5));
  EASSERT(8, sizeof(-10 - (long)5));
  EASSERT(8, sizeof(-10 * (long)5));
  EASSERT(8, sizeof(-10 / (long)5));
  EASSERT(8, sizeof((long)-10 + 5));
  EASSERT(8, sizeof((long)-10 - 5));
  EASSERT(8, sizeof((long)-10 * 5));
  EASSERT(8, sizeof((long)-10 / 5));

  ASSERT(1, ({ char i; sizeof(++i); }));
  ASSERT(1, ({ char i; sizeof(i++); }));

  EASSERT(8, sizeof(int(*)[10]));
  EASSERT(8, sizeof(int(*)[][10]));

  EASSERT(4, sizeof(struct { int x, y[]; }));

  EASSERT(1, sizeof(char));
  EASSERT(1, sizeof(signed char));
  EASSERT(1, sizeof(signed char signed));
  EASSERT(1, sizeof(unsigned char));
  EASSERT(1, sizeof(unsigned char unsigned));

  EASSERT(2, sizeof(short));
  EASSERT(2, sizeof(int short));
  EASSERT(2, sizeof(short int));
  EASSERT(2, sizeof(signed short));
  EASSERT(2, sizeof(int short signed));
  EASSERT(2, sizeof(unsigned short));
  EASSERT(2, sizeof(int short unsigned));

  EASSERT(4, sizeof(int));
  EASSERT(4, sizeof(signed int));
  EASSERT(4, sizeof(signed));
  EASSERT(4, sizeof(signed signed));
  EASSERT(4, sizeof(unsigned int));
  EASSERT(4, sizeof(unsigned));
  EASSERT(4, sizeof(unsigned unsigned));

  EASSERT(8, sizeof(long));
  EASSERT(8, sizeof(signed long));
  EASSERT(8, sizeof(signed long int));
  EASSERT(8, sizeof(unsigned long));
  EASSERT(8, sizeof(unsigned long int));

  EASSERT(8, sizeof(long long));
  EASSERT(8, sizeof(signed long long));
  EASSERT(8, sizeof(signed long long int));
  EASSERT(8, sizeof(unsigned long long));
  EASSERT(8, sizeof(unsigned long long int));

  EASSERT(1, sizeof((char)1));
  EASSERT(2, sizeof((short)1));
  EASSERT(4, sizeof((int)1));
  EASSERT(8, sizeof((long)1));

  EASSERT(4, sizeof((char)1 + (char)1));
  EASSERT(4, sizeof((short)1 + (short)1));
  EASSERT(4, sizeof(1?2:3));
  EASSERT(4, sizeof(1?(short)2:(char)3));
  EASSERT(8, sizeof(1?(long)2:(char)3));

  EASSERT(1, sizeof(char) << 31 >> 31);
  EASSERT(1, sizeof(char) << 63 >> 63);

  EASSERT(4, sizeof(float));
  EASSERT(8, sizeof(double));

  EASSERT(4, sizeof(1.f+2));
  EASSERT(8, sizeof(1.0+2));
  EASSERT(4, sizeof(1.f-2));
  EASSERT(8, sizeof(1.0-2));
  EASSERT(4, sizeof(1.f*2));
  EASSERT(8, sizeof(1.0*2));
  EASSERT(4, sizeof(1.f/2));
  EASSERT(8, sizeof(1.0/2));

  EASSERT(16, sizeof(long double));

  EASSERT(1, sizeof(main));

  printf("OK\n");
  return 0;
}
