#include <fcntl.h>   // here has to be something  

/*@ printf(t, p) =
    REQ  TRUE
    ENS (: TRUE ; printf(p) ; (_)^* ; unit) @*/
