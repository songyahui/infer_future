#include <fcntl.h>   // here has to be something  

/*@ socket(a, b, c) = 
    REQ TRUE
    ENS (∃ l : TRUE ; socket()  ; (!close(l))^* · close(l) ·  (_)^* ; l)  @*/
