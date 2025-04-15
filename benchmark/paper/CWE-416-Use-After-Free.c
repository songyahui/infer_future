//https://cwe.mitre.org/data/definitions/416.html
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

/*@ free(ptr)  = 
    REQ  TRUE
    ENS (∃ r : r=unit ; free(ptr) ; (!_(ptr))^* ; r) @*/

/*@ malloc(size) = 
    REQ size > 0 
    ENS (∃ l : TRUE ; malloc(l)  ; (!free(l))^* · free(l) ·  (_)^* ; l)  @*/

/*@ return(t) =
    REQ  TRUE
    ENS (: TRUE ; 𝝐 ; (_)^* ; t; -1) @*/

/*@ printf(t, p) =
    REQ  TRUE
    ENS (: TRUE ; printf(p) ; (_)^* ; unit) @*/


#define BUFSIZER1 512
#define BUFSIZER2 ((BUFSIZER1/2) - 8)

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
    char *buf1R1;
    char *buf2R1;
    char *buf2R2;
    char *buf3R2;
    buf1R1 = (char *) malloc(BUFSIZER1);
    buf2R1 = (char *) malloc(BUFSIZER1);
    free(buf2R1);
    buf2R2 = (char *) malloc(BUFSIZER2);
    buf3R2 = (char *) malloc(BUFSIZER2);
    strncpy(buf2R1, argv[1], BUFSIZER1-1);
    free(buf1R1);
    free(buf2R2);
    free(buf3R2);
}

