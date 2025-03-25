#include <stdlib.h>

struct st {int flag; void *f;};

void *foo(struct st *p){
  void *q;
  if (p->flag) q = malloc(1);  
  else q = p->f; 
  return q;}

int main() {
  struct st p; 
  void *q;
  p.f = malloc(1);
  q = foo(&p); 
  free(q); 
  //Issue 4: double free
  if (p.flag) free(p.f); 
  return 0; 
}

int test(void *q) {
  q = malloc(1);
  return 1; 
}


int test2(int* q) {
  *q = 1; 
  *q = 2; 
}


int test3(int q) {
  q = 1; 
  q = 2; 
}

//./infer/bin/infer run --pulse-only -- clang++ -c  '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/Fig4.c'