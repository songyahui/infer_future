#include <fcntl.h>   // here has to be something  

/*@ socket(a, b, c) = 
    REQ TRUE
    ENS (∃ l : l>=0 ; socket()  ; (!close(l))^* · close(l) ·  (_)^* ; l)  @*/

/*@ close(fd) = 
    REQ  TRUE
    ENS (∃r : r=unit ; close(fd) ; (_)^* ; r) @*/
