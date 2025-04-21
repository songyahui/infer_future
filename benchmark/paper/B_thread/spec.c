#include <fcntl.h>   // here has to be something  

/*@ pthread_create(a, b, c, d) = 
    REQ TRUE
    ENS (∃ l : TRUE ; pthread_create(a)  ; (!pthread_join(a))^* · pthread_join(a) ·  (_)^* ; l)  @*/

/*@ pthread_join(a, b) =
    REQ  TRUE
    ENS (∃r : TRUE ; pthread_join(a) ; (_)^* ; r) @*/
