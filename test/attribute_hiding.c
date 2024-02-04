typedef union {
  void *ptr;
  int fd;
  unsigned u32;
  unsigned u64;
} Data_t;

struct pre_inc {
  unsigned events;
  Data_t data;
} __attribute__((packed));

#include "test.h"

struct post_inc {
  unsigned events;
  Data_t data;
} __attribute__((packed));

#ifdef __attribute__
#undef __attribute__
#endif

#define __attribute__(x)

struct re_def {
  unsigned events;
  Data_t data;
} __attribute__((packed));


void pre_include(void) {
  DASSERT(sizeof(struct pre_inc) == 12);
  DASSERT(offsetof(struct pre_inc, data) == 4);
}

void post_include(void){
  DASSERT(sizeof(struct post_inc) == 12);
  DASSERT(offsetof(struct post_inc, data) == 4);
}

void redefined(void) {
  DASSERT(sizeof(struct re_def) == 16);
  DASSERT(offsetof(struct re_def, data) == 8);
}

int main() {
  pre_include();
  post_include();
  redefined();

  printf("OK\n");
  return 0;
}
