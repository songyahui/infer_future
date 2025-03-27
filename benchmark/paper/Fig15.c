#include <fcntl.h>    // For open()
#include <unistd.h>   // For write(), close()
#include <stdlib.h>   // For exit()


int current_memory_use (); 

void test() {
    int a = current_memory_use (); 
    int b = current_memory_use (); 
    
}
