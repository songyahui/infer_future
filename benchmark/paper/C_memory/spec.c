#include <fcntl.h>   // here has to be something  

/*@ free(ptr)  = 
    REQ  TRUE
    ENS (∃ r : r=unit ; free(ptr) ; (!_(ptr))^* ; r) @*/

/*@ malloc(size) = 
    REQ TRUE
    ENS (∃ l : TRUE ; malloc(l)  ; (!free(l))^* · free(l) ·  (_)^* ; l)  @*/

/*@ realloc(ptr, size) = 
    REQ TRUE
    ENS (∃ l : TRUE ; realloc(l)  ; (!_(ptr))^* /\ (!free(l))^* · free(l) ·  (_)^* ; l)  @*/


/*@ return(t) =
    REQ  TRUE
    ENS (: TRUE ; 𝝐 ; (_)^* ; t; -1) @*/


/*@ printf(t, p) =
    REQ  TRUE
    ENS (: TRUE ; printf(p) ; (_)^* ; unit) @*/

/*@ strncpy(a, b, c) =
    REQ  TRUE
    ENS (: TRUE ; strncpy(a) ; (_)^* ; unit) @*/

