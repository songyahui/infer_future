#include <fcntl.h>    // For open()
#include <unistd.h>   // For write(), close()
#include <stdlib.h>   // For exit()

int test(int x) 
{ 

    int a = 1; 
    a = x + 1; 
    a = a + 2;
    return x; 
}

void open_and_write(const char* path) 
{ 
    int fd = open(path, O_RDONLY); 
    if (fd==-1) exit(-1); 
    ssize_t bytes = write(fd, "m", 1); 
    close(fd);
    return; 
}


void open_and_write1(const char* path) {
    // Open the file in write-only mode, create it if it doesn't exist, and truncate it if it does.
    int fd = open(path, O_RDONLY);
    if (fd == -1) {
        // If opening the file fails, exit with an error code.
        exit(-1);
    }

    // Write a single character 'm' to the file.
    ssize_t bytes = write(fd, "m", 1);
    if (bytes == -1) {
        // If writing fails, close the file and exit with an error code.
        close(fd);
        exit(-1);
    }

    // Close the file descriptor.
    close(fd);
}

//infer/bin/infer run -- clang -c benchmark/paper/Fig2.c