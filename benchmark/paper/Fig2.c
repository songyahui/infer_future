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
    ENS (: TRUE ; exit() ; (_)^* ; code; -2 ) @*/

/*@ return(t) =
    REQ  TRUE
    ENS (: TRUE ; 𝝐 ; (_)^* ; t; -1) @*/

void test0(const char* path) 
{ 
    int fd = open(path, O_RDONLY); 
    return; 
}


int test1(const char* path) 
{ 
    int fd = open(path, O_RDONLY); 
    return fd; 
}


void open_and_write(const char* path) 
{ 
    void* buf;  
    int fd = open(path, O_RDONLY); 
    if (fd==-1) return; 
    ssize_t bytes1 = read(fd, buf, 1); 
    ssize_t bytes2 = write(fd, buf, 1); 
    close(fd);
    return; 
}

void open_and_read(const char* path) 
{ 
    void* buf;  
    int fd = open(path, O_WRONLY); 
    if (fd==-1) return; 
    ssize_t bytes1 = read(fd, buf, 1); 
    ssize_t bytes2 = write(fd, buf, 1); 
    close(fd);
    return; 
}

void open_and_read_and_write(const char* path) 
{ 
    void* buf;  
    int fd = open(path, O_RDWR); 
    if (fd==-1) return; 
    ssize_t bytes1 = read(fd, buf, 1); 
    ssize_t bytes2 = write(fd, buf, 1); 
    close(fd);
    return; 
}


void open_and_write1(const char* path) {
    // Open the file in write-only mode, create it if it doesn't exist, and truncate it if it does.
    void* buf;  

    int fd = open(path, O_RDWR);
    if (fd == -1) {
        // If opening the file fails, exit with an error code.
        return; 
    }

    // Write a single character 'm' to the file.
    ssize_t bytes = write(fd, "m", 1);
    if (bytes == -1) {
        // If writing fails, close the file and exit with an error code.
        close(fd);
        return; 
    }

    // Close the file descriptor.
    close(fd);
}


//infer/bin/infer run -- clang -c benchmark/paper/Fig2.c