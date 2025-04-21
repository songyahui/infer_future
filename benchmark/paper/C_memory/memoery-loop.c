#include <stdlib.h>

    
void test1 () {
    void *arr;
    for (int i = 0; i < 10; i++) {
        void *data = malloc(100 * sizeof(int)); // Allocated each iteration [4][6]
        // Missing free(data)
    }
}

void test2 () {
    void *buffer = NULL;
    while (1) {
        buffer = malloc(50); // Previous allocation lost [4][8]
        // ...
    }
    free(buffer); // Only releases last allocation
}


void test3 () {
    void *arr;
    for (int i = 0; i < 10; i++) {
        arr = malloc(50 * sizeof(int)); // New allocation each iteration
    }
    free(arr); // Only frees final allocation [6]
}

void test4 () {
    void *arr;
    int error_check; 
    while (1) {
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

