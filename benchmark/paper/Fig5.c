#include <fcntl.h>    
#include <unistd.h>   
#include <stdlib.h>   
#define NULL 0

/*@ free(ptr)  = 
    REQ  TRUE
    ENS (∃ r : r=unit ; free(ptr) ; (!_(ptr))^* ; r) @*/

/*@ malloc(size) = 
    REQ size > 0 
    ENS (∃ l : TRUE ; malloc(l)  ; (!free(l))^* · free(l) ·  (_)^* ; l)  @*/

/*@ return(t) =
    REQ  TRUE
    ENS (: TRUE ; 𝝐 ; (_)^* ; t; -1) @*/



void open_and_closeN_v1(int n, char** paths) {
  int fd[n]; // Stack-allocated array

  int i = 0; 
  while (i < n) 
  {
    fd[i] = open(paths[i], O_RDWR);
    i = i + 1; 
  }

  int k = 0 ; 
  while (k < n) 
  {
    if (fd[k]==-1) { k = k + 1;}
    else 
      {close(fd[k]); k = k + 1;}
  }
}

void mallocN (void **arr, int n) {
  int i = 0; 
  while (i < n) {
    arr[i] = malloc(4);
    i = i + 1; }
  return ;}

int main () {
  void *array[5];
  mallocN (array, 5); 
  free(array[0]);
}
//./infer/bin/infer run --pulse-only -- clang++ -c  '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/Fig4.c'
