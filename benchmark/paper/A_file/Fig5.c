#include <fcntl.h>    
#include <unistd.h>   
#include <stdlib.h>   


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

//./infer/bin/infer run --pulse-only -- clang++ -c  '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/Fig4.c'
