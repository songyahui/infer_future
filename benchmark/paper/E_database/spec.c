#include <fcntl.h>   // here has to be something  

/*@ sqlite3_open(a, b) = 
    REQ TRUE
    ENS (∃ l : TRUE ; sqlite3_open(b)  ; (!sqlite3_close(b))^* · sqlite3_close(b) ·  (_)^* ; l)  @*/

/*@ sqlite3_close(a) =
    REQ  TRUE
    ENS (∃r : TRUE ; sqlite3_close(a) ; (_)^* ; r) @*/
