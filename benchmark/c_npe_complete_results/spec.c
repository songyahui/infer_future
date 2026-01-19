#include <fcntl.h>   // here has to be something  

/*@ free(ptr)  =
    REQ  TRUE
    ENS (∃ r : r=unit ; free(ptr) ; (!_(ptr))^* ; r) @*/

/*@ malloc(size) =
    REQ TRUE
    ENS (∃ l : !(l=0) ; malloc(l)  ; (!free(l))^* · free(l) ·  (_)^* ; l)  @*/

/*@ realloc(ptr, size) =
    REQ TRUE
    ENS (∃ l : !(l=0) ; realloc(l)  ; (!_(ptr))^* /\ (!free(l))^* · free(l) ·  (_)^* ; l) \/ 
        (∃ l : l=0 ; 𝝐  ; (!_(l))^*  ; l) @*/
