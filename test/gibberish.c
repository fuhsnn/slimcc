#include "test.h"

void fn_param(void) {
    void (*A) (int (*pf)(char));
    void (*B) (int *f(char));
    _Static_assert(_Generic(A, typeof(&B):0, default: 1));
    _Static_assert(_Generic(B, typeof(&A):0, default: 1));


    _Static_assert(_Generic(A, void(*)(int         (char) ):1));
    _Static_assert(_Generic(A, void(*)(int        ((char))):1));
    _Static_assert(_Generic(A, void(*)(int    p    (char) ):1));
    _Static_assert(_Generic(A, void(*)(int(   p  ) (char) ):1));
    _Static_assert(_Generic(A, void(*)(int( ( p )) (char) ):1));
    _Static_assert(_Generic(B, void(*)(int   *     (char) ):1));
    _Static_assert(_Generic(B, void(*)(int   *    ((char))):1));
    _Static_assert(_Generic(A, void(*)(int(  *   ) (char) ):1));
    _Static_assert(_Generic(A, void(*)(int( (*  )) (char) ):1));

    _Static_assert(_Generic(B, void(*)(int*   p    (char) ):1));
    _Static_assert(_Generic(B, void(*)(int*(  p  ) (char) ):1));
    _Static_assert(_Generic(B, void(*)(int*(( p )) (char) ):1));

    _Static_assert(_Generic(A, void(*)(int(  *p  ) (char) ):1));
    _Static_assert(_Generic(A, void(*)(int( (*p )) (char) ):1));
    _Static_assert(_Generic(A, void(*)(int(*( p )) (char) ):1));
    _Static_assert(_Generic(A, void(*)(int(*((p))) (char) ):1));

    _Static_assert(_Generic(A, void(*)(int(   p    (char) )):1));
    _Static_assert(_Generic(A, void(*)(int( ( p )  (char) )):1));
    _Static_assert(_Generic(A, void(*)(int( ((p))  (char) )):1));
    _Static_assert(_Generic(B, void(*)(int(  *     (char) )):1));
    _Static_assert(_Generic(B, void(*)(int(  *    ((char)))):1));
    _Static_assert(_Generic(A, void(*)(int( (*  )  (char) )):1));
    _Static_assert(_Generic(A, void(*)(int(((*  )) (char) )):1));

    _Static_assert(_Generic(B, void(*)(int(*  p    (char))):1));
    _Static_assert(_Generic(B, void(*)(int(*( p )  (char))):1));
    _Static_assert(_Generic(A, void(*)(int( (*p )  (char))):1));
    _Static_assert(_Generic(B, void(*)(int(*((p))  (char))):1));
    _Static_assert(_Generic(A, void(*)(int((*(p))  (char))):1));
    _Static_assert(_Generic(A, void(*)(int(((*p))  (char))):1));
}

void fn_param_empty(void) {
    void (*A) (int (*pf)());
    void (*B) (int *f());
    _Static_assert(_Generic(A, typeof(&B):0, default: 1));
    _Static_assert(_Generic(B, typeof(&A):0, default: 1));


    _Static_assert(_Generic(A, void(*)(int         () ):1));
    _Static_assert(_Generic(A, void(*)(int        (())):1));
    _Static_assert(_Generic(A, void(*)(int    p    () ):1));
    _Static_assert(_Generic(A, void(*)(int(   p  ) () ):1));
    _Static_assert(_Generic(A, void(*)(int( ( p )) () ):1));
    _Static_assert(_Generic(B, void(*)(int   *     () ):1));
    _Static_assert(_Generic(B, void(*)(int   *    (())):1));
    _Static_assert(_Generic(A, void(*)(int(  *   ) () ):1));
    _Static_assert(_Generic(A, void(*)(int( (*  )) () ):1));

    _Static_assert(_Generic(B, void(*)(int*   p    () ):1));
    _Static_assert(_Generic(B, void(*)(int*(  p  ) () ):1));
    _Static_assert(_Generic(B, void(*)(int*(( p )) () ):1));

    _Static_assert(_Generic(A, void(*)(int(  *p  ) () ):1));
    _Static_assert(_Generic(A, void(*)(int( (*p )) () ):1));
    _Static_assert(_Generic(A, void(*)(int(*( p )) () ):1));
    _Static_assert(_Generic(A, void(*)(int(*((p))) () ):1));

    _Static_assert(_Generic(A, void(*)(int(   p    () )):1));
    _Static_assert(_Generic(A, void(*)(int( ( p )  () )):1));
    _Static_assert(_Generic(A, void(*)(int( ((p))  () )):1));
    _Static_assert(_Generic(B, void(*)(int(  *     () )):1));
    _Static_assert(_Generic(B, void(*)(int(  *    (()))):1));
    _Static_assert(_Generic(A, void(*)(int( (*  )  () )):1));
    _Static_assert(_Generic(A, void(*)(int(((*  )) () )):1));

    _Static_assert(_Generic(B, void(*)(int(*  p    ())):1));
    _Static_assert(_Generic(B, void(*)(int(*( p )  ())):1));
    _Static_assert(_Generic(A, void(*)(int( (*p )  ())):1));
    _Static_assert(_Generic(B, void(*)(int(*((p))  ())):1));
    _Static_assert(_Generic(A, void(*)(int((*(p))  ())):1));
    _Static_assert(_Generic(A, void(*)(int(((*p))  ())):1));
}

void fn_param_typedef(void) {
    typedef struct {
      int i;
    } S;

    void (*A) (int (*pf)(S));
    void (*B) (int *f(S));
    _Static_assert(_Generic(A, typeof(&B):0, default: 1));
    _Static_assert(_Generic(B, typeof(&A):0, default: 1));

    _Static_assert(_Generic(A, void(*)(int         (S) ):1));
    _Static_assert(_Generic(A, void(*)(int        ((S))):1));
    _Static_assert(_Generic(A, void(*)(int    p    (S) ):1));
    _Static_assert(_Generic(A, void(*)(int(   p  ) (S) ):1));
    _Static_assert(_Generic(A, void(*)(int( ( p )) (S) ):1));
    _Static_assert(_Generic(B, void(*)(int   *     (S) ):1));
    _Static_assert(_Generic(B, void(*)(int   *    ((S))):1));
    _Static_assert(_Generic(A, void(*)(int(  *   ) (S) ):1));
    _Static_assert(_Generic(A, void(*)(int( (*  )) (S) ):1));

    _Static_assert(_Generic(B, void(*)(int*   p    (S) ):1));
    _Static_assert(_Generic(B, void(*)(int*(  p  ) (S) ):1));
    _Static_assert(_Generic(B, void(*)(int*(( p )) (S) ):1));

    _Static_assert(_Generic(A, void(*)(int(  *p  ) (S) ):1));
    _Static_assert(_Generic(A, void(*)(int( (*p )) (S) ):1));
    _Static_assert(_Generic(A, void(*)(int(*( p )) (S) ):1));
    _Static_assert(_Generic(A, void(*)(int(*((p))) (S) ):1));

    _Static_assert(_Generic(A, void(*)(int(   p    (S) )):1));
    _Static_assert(_Generic(A, void(*)(int( ( p )  (S) )):1));
    _Static_assert(_Generic(A, void(*)(int( ((p))  (S) )):1));
    _Static_assert(_Generic(B, void(*)(int(  *     (S) )):1));
    _Static_assert(_Generic(B, void(*)(int(  *    ((S)))):1));
    _Static_assert(_Generic(A, void(*)(int( (*  )  (S) )):1));
    _Static_assert(_Generic(A, void(*)(int(((*  )) (S) )):1));

    _Static_assert(_Generic(B, void(*)(int(*  p    (S))):1));
    _Static_assert(_Generic(B, void(*)(int(*( p )  (S))):1));
    _Static_assert(_Generic(A, void(*)(int( (*p )  (S))):1));
    _Static_assert(_Generic(B, void(*)(int(*((p))  (S))):1));
    _Static_assert(_Generic(A, void(*)(int((*(p))  (S))):1));
    _Static_assert(_Generic(A, void(*)(int(((*p))  (S))):1));
}

int main(void) {
  printf("OK\n");
  return 0;
}
