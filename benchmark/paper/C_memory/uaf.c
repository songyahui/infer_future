// https://github.com/shahaf-sameach/UseAfterFree/blob/master/uaf.c

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <stdio.h>


struct auth {
        char name[32];
        int auth;
};

struct auth *auth;
char *service;

int main(int argc, char **argv)
{
        char line[128];
        int a, b, c, d, e, f, g, n; 

        printf("[ auth = %p, service = %p ]\n", auth, service);

        //a = (fgets(line, sizeof(line), stdin) == NULL); 
        if(a) return;
                
        //b  = (strncmp(line, "auth ", 5) == 0 ); 
        if(b) {
                n = sizeof(auth); 
                auth = (struct auth*) malloc(n);
                memset(auth, 0, sizeof(auth));
                c = strlen(line + 5) < 31 ; 
                if(c) {
                        strcpy(auth->name, line + 5);
                }
        }
        //d = (strncmp(line, "reset", 5) == 0); 
        if(d) {
                free(auth);
        }
        // e = (strncmp(line, "service", 6) == 0); 
        if(e) {
                service = strdup(line + 7);
        }
        // f = (strncmp(line, "login", 5) == 0); 
        if(f) {
                if(auth->auth) {
                        printf("you have logged in already!\n", auth);
                } else {
                        printf("please enter your password\n");
                }
        }

}
// one memory leak at line 55 
// one use after free at line 49