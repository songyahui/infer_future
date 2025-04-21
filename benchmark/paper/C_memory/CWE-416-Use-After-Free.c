//https://cwe.mitre.org/data/definitions/416.html
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>



#define BUFSIZER1 4

int uaf () {
    int err; 
    int abrt; 
    char* ptr = (char*)malloc (BUFSIZER1);
    if (err) {
        abrt = 1;
        free(ptr);
    }

    if (abrt) {
        printf("operation aborted before commit", ptr);
    }
}


int main(int argc, char **argv) {
    char *buf1R1, *buf2R1, *buf2R2, *buf3R2;
    buf1R1 = (char *) malloc(BUFSIZER1);
    buf2R1 = (char *) malloc(BUFSIZER1);
    free(buf2R1);
    buf2R2 = (char *) malloc(BUFSIZER1);
    buf3R2 = (char *) malloc(BUFSIZER1);
    strncpy(buf2R1, argv[1], BUFSIZER1); // Use-after-free
    free(buf1R1); free(buf2R2); free(buf3R2);
}

int paper(int argc, char **argv) {
    char *buf1R1, *buf2R1, *buf2R2;
    buf1R1 = (char *) malloc(1);
    buf2R1 = (char *) malloc(1);
    free(buf2R1); // free buf2R1
    buf2R2 = (char *) malloc(1);
    strncpy(buf2R1,argv[1],1); //UAF
    free(buf1R1); free(buf2R2); }
