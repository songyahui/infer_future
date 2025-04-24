#include <stdlib.h>

int main() {

    char *ptr = (char*) malloc(10 * sizeof(char));

    // First free

    free(ptr); 

    // ptr is now dangling, but not set to NULL.

    // Second free on the same pointer, leading to double-free.

    free(ptr); 

    return 0;

}

void process_arr(int** arr, int n) {
    free(*arr);
  }

int test (int argc, char* argv[]) {
    int* arr = malloc(sizeof(int) * 10);
    process_arr(&arr, 10);
    free(arr);
    return 0;
  }