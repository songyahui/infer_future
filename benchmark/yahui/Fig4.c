// #include <stdlib.h>

struct st {int flag; int *f;};

int *foo(struct st *p){
  int *q;
  if (p->flag) q = malloc(1);  
  else q = p->f; 
  return q;}

void main() {
  struct st p; int *q;
  p.f = malloc(1);
  q = foo(&p); 
  free(q); 
  //Issue 4: double free
  free(p.f); }