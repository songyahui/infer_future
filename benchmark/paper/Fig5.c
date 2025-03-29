#include <fcntl.h>    
#include <unistd.h>   
#include <stdlib.h>   

/*@ open(path, flag)  = 
    REQ TRUE
    ENS (: r=-1 ; 𝝐 ; exit(-1) · (_)^* ; r)  
     \/ (∃fd  : !(r=-1) ∧ flag=0 ; open(fd) ; (!write(fd))^* /\ (!close(fd))^* · close(fd) ·  (_)^* ; fd) 
     \/ (∃fd : !(r=-1) ∧ flag=1 ; open(fd) ; (!read(fd))^* /\ (!close(fd))^* · close(fd) ·  (_)^* ; fd) 
     \/ (∃fd : !(r=-1) ∧ flag=2 ; open(fd) ; (!close(fd))^* · close(fd) ·  (_)^* ; fd)  @*/

/*@ close(fd) = 
    REQ  TRUE
    ENS (∃r : r=unit ; close(fd) ; (!_(fd))^* ; r) @*/

/*@ write(fd, buf, size) =
    REQ  TRUE
    ENS (∃r : !(r=-1) ; write(fd) ;  (_)^* ; r) 
    \/ (∃r : r=-1 ; 𝝐 ; close(fd) · exit(-1) · (_)^* ; r)  @*/

/*@ read(fd, buf, size) =
    REQ  TRUE
    ENS (∃r : TRUE ; read(fd) ; (_)^* ; r) @*/

/*@ exit(code) =
    REQ  TRUE
    ENS (∃r : r=code ; exit(code) ; (_)^* ; r; -2 ) @*/

/*@ return(t) =
    REQ  TRUE
    ENS (: TRUE ; 𝝐 ; (_)^* ; t; -1) @*/


void open_and_closeN_v1(int n, char** paths) {
  int fd[n]; // Stack-allocated array

  int i = 0; 
  while (i < n) {
    fd[i] = open(paths[i], O_RDONLY);
    i = i + 1; 
  }

  int j = 0 ; 
  while (j < n) {
    close(fd[j]);
  }
}

void open_and_closeN_v2(int n, char** paths) {
  int fd[n]; // Stack-allocated array

  int i = 0; 
  while (i < n) {
    fd[i] = open(paths[i], O_RDONLY);
    if (fd[i] < 0) {exit (-1);}
    else {close(fd[i]);}
    i = i + 1; 
  }
}


//infer/bin/infer run -- clang -c benchmark/paper/Fig2.c