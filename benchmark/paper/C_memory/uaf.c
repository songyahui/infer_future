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

                if(fgets(line, sizeof(line), stdin) == NULL) return;
                
                if(strncmp(line, "auth ", 5) == 0) {
                        auth = malloc(sizeof(auth));
                        memset(auth, 0, sizeof(auth));
                        if(strlen(line + 5) < 31) {
                                strcpy(auth->name, line + 5);
                        }
                }
                if(strncmp(line, "reset", 5) == 0) {
                        free(auth);
                }
                if(strncmp(line, "service", 6) == 0) {
                        service = strdup(line + 7);
                }
                if(strncmp(line, "login", 5) == 0) {
                        int t = auth->auth; 
                        if(t) {
                                printf("you have logged in already!\n");
                        } else {
                                printf("please enter your password\n");
                        }
                }

}
// one memory leak at line 55 
// one use after free at line 49