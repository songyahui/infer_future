// https://github.com/shahaf-sameach/UseAfterFree/blob/master/uaf.c

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <stdio.h>

/*@ free(ptr)  = 
    REQ  TRUE
    ENS (∃ r : r=unit ; free(ptr) ; (!_(ptr))^* ; r) @*/

/*@ malloc(size) = 
    REQ size > 0 
    ENS (∃ l : TRUE ; malloc(l)  ; (!free(l))^* · free(l) ·  (_)^* ; l)  @*/

/*@ return(t) =
    REQ  TRUE
    ENS (: TRUE ; 𝝐 ; (_)^* ; t; -1) @*/


struct auth {
        char name[32];
        int auth;
};

struct auth *auth;
char *service;

int main(int argc, char **argv)
{
        char line[128];

        //while(1) {
                printf("[ auth = %p, service = %p ]\n", auth, service);

                if(fgets(line, sizeof(line), stdin) == NULL) return;
                
                if(strncmp(line, "auth ", 5) == 0) {
                        auth = (struct auth*) malloc(sizeof(auth));
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
                        if(auth->auth) {
                                printf("you have logged in already!\n");
                        } else {
                                printf("please enter your password\n");
                        }
                }
        //}
}