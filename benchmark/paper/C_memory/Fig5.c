#include <fcntl.h>    
#include <unistd.h>   
#include <stdlib.h>   
#define NULL 0



void mallocN (void **arr, int n) {
  int i = 0; 
  while (i < n) {
    arr[i] = malloc(4);
    i = i + 1; }
  return ;}

int main () {
  void *array[5];
  mallocN (array, 5); 
  free(array[0]); // memory leak 
}
//./infer/bin/infer run --pulse-only -- clang++ -c  '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/Fig4.c'
