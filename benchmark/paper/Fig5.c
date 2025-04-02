#include <fcntl.h>    
#include <unistd.h>   
#include <stdlib.h>   

/*@ open(path, flag)  = 
    REQ TRUE
    ENS (∃fd: fd=-1 ; 𝝐 ; (!_(fd))^* ; fd)  
     \/ (∃fd  : !(fd=-1) ∧ flag=0 ; open(fd) ; (!write(fd))^* /\ (!close(fd))^* · close(fd) ·  (_)^* ; fd) 
     \/ (∃fd : !(fd=-1) ∧ flag=1 ; open(fd) ; (!read(fd))^* /\ (!close(fd))^* · close(fd) ·  (_)^* ; fd) 
     \/ (∃fd : !(fd=-1) ∧ flag=2 ; open(fd) ; (!close(fd))^* · close(fd) ·  (_)^* ; fd)  @*/

/*@ close(fd) = 
    REQ  TRUE
    ENS (∃r : r=unit ; close(fd) ; (!_(fd))^* ; r) @*/

/*@ write(fd, buf, size) =
    REQ  TRUE
    ENS (∃r : TRUE ; write(fd) ; (_)^* ; r) @*/

/*@ read(fd, buf, size) =
    REQ  TRUE
    ENS (∃r : TRUE ; read(fd) ; (_)^* ; r) @*/

/*@ exit(code) =
    REQ  TRUE
    ENS (∃r : r=code ; exit() ; (_)^* ; r; -2 ) @*/

/*@ return(t) =
    REQ  TRUE
    ENS (: TRUE ; 𝝐 ; (_)^* ; t; -1) @*/


void open_and_closeN_v1(int n, char** paths) {
  int fd[n]; // Stack-allocated array

  int i = 0; 
  while (i < n) 
  {
    fd[i] = open(paths[i], O_RDWR);
    // i = i + 1; 
  }

  /*
  int k = 0 ; 
  while (k < n) 
  {
    if (fd==-1) { k = k + 1;}
    else 
      {close(fd[k]); k = k + 1;}
  }
  */

}


/*
define O_RDONLY        0x0000          /* open for reading only 
define O_WRONLY        0x0001          /* open for writing only 
define O_RDWR          0x0002          /* open for reading and writing 
define O_ACCMODE       0x0003          /* mask for above modes 
*/