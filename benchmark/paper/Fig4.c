#include <stdlib.h>

struct st {int flag; void *f;};

void *foo(struct st *p){
  void *q;
  if (p->flag) q = malloc(1);  
  else q = p->f; 
  return q;}

int main() {
  struct st p; void *q;
  p.f = malloc(1);
  q = foo(&p); 
  free(q); 
  //Issue 4: double free
  free(p.f); 
  return 0; 
}