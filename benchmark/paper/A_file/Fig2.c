#include <fcntl.h>    
#include <unistd.h>   
#include <stdlib.h>   

void assumeF(const char* path) ; 

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

int test2(const char* path) 
{ 
    int fd = open(path, O_RDONLY); 
    if (fd==-1) {
        int r = -1; 
        assumeF("(!_(r))^*");
        return r; }
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

void open_and_read_and_write_extra(const char* path) 
{ 
    void* buf;  
    if (path == "") {
        int fd = open(path, O_RDWR); 
        if (fd==-1) return; 
        ssize_t bytes1 = read(fd, buf, 1); 
        ssize_t bytes2 = write(fd, buf, 1); 
        close(fd);
        return; }
    else {
        int fd = open(path, O_WRONLY); 
        if (fd==-1) return; 
        ssize_t bytes1 = read(fd, buf, 1); 
        ssize_t bytes2 = write(fd, buf, 1); 
        close(fd);
        return; 
    }
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