#include <stdlib.h>

void false_positive() {
  int** ptr1 = (int**) malloc(4);
  int* ptr2 = (int*) malloc(4);
  *ptr1 = ptr2;
  free(*ptr1);
  free(ptr1); }