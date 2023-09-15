#include "test.h"
#include <stdint.h>

int main(void){
  ASSERT(2, ({ uint32_t i=0; switch(i){case 0 ...0xFFFFFFFF: i=2;} i; }));
  ASSERT(2, ({ int32_t i=0; switch(i){case 0x80000000 ...0x7FFFFFFF: i=2;} i; }));
  ASSERT(2, ({ uint64_t i=0; switch(i){case 0 ...0xFFFFFFFFFFFFFFFF: i=2;} i; }));
  ASSERT(2, ({ int64_t i=0; switch(i){case 0x8000000000000000 ...0x7FFFFFFFFFFFFFFF: i=2;} i; }));

  ASSERT(2, ({ uint32_t i=0; switch(i){case 0 ...0x100000000: i=2;} i; }));
  ASSERT(2, ({ int32_t i=-1; switch(i){case -1 ...(int64_t)-1: i=2;} i; }));

  ASSERT(2, ({ int64_t i=0; switch(0x123456789){case 0x123456789: i=2;} i; }));

  printf("OK\n");
}
