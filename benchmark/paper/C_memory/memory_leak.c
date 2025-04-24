#include <stdlib.h>
#include <string.h>


int test(int argc, char *argv[]) {
    char *string, *string_so_far;
    int i, length; 
    i = 0;    
    length = 0;
    while (i<argc) { // 1.  memory leak 
        length += strlen(argv[i])+1;
        string = malloc(1);
 
        /*  * Copy the string built so far. */ 
        if(string_so_far != (char *)0)
            strcpy(string, string_so_far);
        else *string = '\0';
        strcat(string, argv[i]);
        if (i < argc-1) strcat(string, " "); 
         
        string_so_far = string; 
        i = i + 1; 
    } 
    printf("You entered: %s\n", string_so_far);
    return (0);
}



int test1(int argc, char *argv[]) {
    char *string, *string_so_far;
    int i, length; 
    i = 0;    
    length = 0;
    while (i<argc) {
        length += strlen(argv[i])+1;
        string = malloc(1);
        free(string);
        /*  * Copy the string built so far. */ 
        if(string_so_far != (char *)0)
            strcpy(string, string_so_far);
        else *string = '\0';
        strcat(string, argv[i]);
        if (i < argc-1) strcat(string, " "); 
         
        string_so_far = string; 
        
        i = i + 1; 
    }
    printf("You entered: %s\n", string_so_far);
    return (0);
}

int test2(int argc, char *argv[]) {
    char *string, *string_so_far;
    int i, length; 
    i = 0;    
    length = 0;
    while (i<argc) {
        length += strlen(argv[i])+1;
        string = malloc(1);
 
         
        string_so_far = string; 
        free(string_so_far); 
        i = i + 1; 
    }
    printf("You entered: %s\n", string_so_far);
    return (0);
}

int test3(int argc, char *argv[]) {
    char *string, *string_so_far;
    int i, length; 
    i = 0;    
    length = 0;
    while (i<argc) { // 2.  memory leak 
        length += strlen(argv[i])+1;
        string = malloc(1);
        /*  * Copy the string built so far. */ 
        if(string_so_far != (char *)0)
            strcpy(string, string_so_far);
        else *string = '\0';
        strcat(string, argv[i]);
        if (i < argc-1) strcat(string, " "); 
         
        string_so_far = string; 
        free(string_so_far); 

        i = i + 1; 
    }
    printf("You entered: %s\n", string_so_far);
    return (0);
}

void main() {
    char *string, *string_so_far;
    string = malloc(1);
    string_so_far = string; 
    free(string_so_far);
    printf("You entered: %s\n", string); // 3.  use after free 
}




void leakMemory() {

    char *ptr = (char*)malloc(10 * sizeof(char)); // Memory is allocated

    // some application logic executes here using the allocated memory

    // Missing free() call when memory is no longer needed, leading to memory leak
    // 4. memory leak 

}

int main1() {

    for(int i = 0; i < 1000; i++) {

        leakMemory();

    }

    // The allocated memory in leakMemory is never freed.

    return 0;

}