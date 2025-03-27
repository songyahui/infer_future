#include <stdlib.h>

/*@ free(ptr)  = 
    REQ  TRUE
    ENS (∃ r : r=unit ; free(ptr) ; (!_(ptr))^* ; r) @*/

/*@ malloc(size) = 
    REQ size > 0 
    ENS (∃ l : TRUE ; malloc(l)  ; (!free(l))^* · free(l) ·  (_)^* ; l)  @*/

/*@ return(t) =
    REQ  TRUE
    ENS (: TRUE ; 𝝐 ; (_)^* ; t; -1) @*/


struct st {int flag; void *f;};

void *foo(struct st *p) {
  void *q;
  if (p->flag) q = malloc(1);  
  else q = p->f; 
  return q; 
}

int main() {
  struct st p; 
  void *q;
  p.f = malloc(1);
  q = foo(&p); 
  free(q); 
  //Issue 4: double free
  //if (p.flag) 
  free(p.f); 
  return 0; 
}

int test(void* q) {
  q = malloc(1);
  return 1; 
}

int test1() {
  void* q = malloc(1);
  return 1; 
}

//./infer/bin/infer run --pulse-only -- clang++ -c  '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/Fig4.c'

// jit