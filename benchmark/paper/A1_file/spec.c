#include <fcntl.h>   // here has to be something  


/*@ open(path, flag)  = 
    REQ TRUE
    ENS (∃fd : !(fd=-1); open(fd) ; (!close(fd))^* · close(fd) ·  (_)^* ; fd)   @*/

/*@ fopen(path, flag)  = 
    REQ TRUE
    ENS  (∃fd : !(fd=-1) ; open(fd) ; (!close(fd))^* · close(fd) ·  (_)^* ; fd)   @*/


/*@ close(fd) = 
    REQ  TRUE
    ENS (∃r : r=unit ; close(fd) ; (_)^* ; r) @*/

/*@ fclose(fd) = 
    REQ  TRUE
    ENS (∃r : r=unit ; close(fd) ; (_)^* ; r) @*/


/*@ write(fd, buf, size) =
    REQ  TRUE
    ENS (∃r : TRUE ; write(fd) ; (_)^* ; r) @*/

/*@ read(fd, buf, size) =
    REQ  TRUE
    ENS (∃r : TRUE ; read(fd) ; (_)^* ; r) @*/

/*@ exit(code) =
    REQ  TRUE
    ENS (: TRUE ; exit() ; (_)^* ; code; -2 ) @*/

/*@ return(t) =
    REQ  TRUE
    ENS (: TRUE ; 𝝐 ; (_)^* ; t; -1) @*/