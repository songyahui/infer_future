#include <stdlib.h>

void false_positive() {
  int** ptr1 = (int**) malloc(4);
  *ptr1 = (int*) malloc(4);  
  free(*ptr1);
  free(ptr1); }