#include <stdlib.h>

    
void test1 () {
    void *arr;
    for (int i = 0; i < 10; i++) { //1. memory leak 
        void *data = malloc(100 * sizeof(int)); // Allocated each iteration [4][6]
        // Missing free(data)
    }
}

void test2 () {
    void *buffer = NULL;
    for (int i = 0; i < 10; i++) { //2. memory leak 
        buffer = malloc(50); // Previous allocation lost [4][8]
        // ...
    }
    free(buffer); // Only releases last allocation
}


void test3 () {
    void *arr;
    for (int i = 0; i < 10; i++) { //3. memory leak 
        arr = malloc(50 * sizeof(int)); // New allocation each iteration
    }
    free(arr); // Only frees final allocation [6]
}

void test4 () {
    void *arr;
    int error_check; 
    for (int i = 0; i < 10; i++) { //3. memory leak 
        void *resource = malloc(50);
        if (error_check) break; // Exit before free
        // ...
        free(resource); // Skipped on break [3][8]
    }
}

void test5 () {
    int OUTER, INNER, ROWS, COLS; 
    for (int i = 0; i < OUTER; i++) {
        for (int j = 0; j < INNER; j++) { 
            void *matrix = malloc(ROWS * COLS * sizeof(int)); // Nested leak [7]
        }
    }
}