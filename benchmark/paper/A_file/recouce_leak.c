#include <stdio.h>
#include <stdlib.h>  

int main(int argc, char **argv) {
    char filename[100];

    for (size_t i = 0; i < 1500; ++i) {
        sprintf(filename, "/tmp/%010ld.txt", i);
        FILE *fp = fopen(filename, 2);

        if (fp == NULL) {
            printf("i = %6ld: failed!\n", i);
            return 1;
        }

        if (i == 0) {
            printf("i = %6ld: ok.\n", i);
        }
    }

    return 0;
}