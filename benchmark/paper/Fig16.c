#include <fcntl.h>    
#include <unistd.h>   
#include <stdlib.h>   

#include <fcntl.h>    
#include <unistd.h>   
#include <stdlib.h>   

void open_and_closeN_v2(int n, char** paths) {
  int fd[n]; // Stack-allocated array

  int i = 0; 
  while (i < n) {
    fd[i] = open(paths[i], O_RDWR);
    if (fd[i] == -1 ) {return;}
    close(fd[i]);
    i = i + 1; 
  }
}


//infer/bin/infer run -- clang -c benchmark/paper/Fig2.c