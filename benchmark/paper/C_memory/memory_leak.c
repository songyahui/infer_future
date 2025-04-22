#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[]) {
    char *string, *string_so_far;
    int i, length; 
    i = 0;    
    length = 0;
    while (i<argc) {
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