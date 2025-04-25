#include <fcntl.h>   // here has to be something  

/*@ fgets(a, b, c)  = 
    REQ  TRUE
    ENS (∃ r : r=NULL ; fgets(a) ; return() ; r) @*/

/*@ returnChunkSize(a)  = 
    REQ  TRUE
    ENS (∃ r : r=-1 ; returnChunkSize(a) ; return() ; r) @*/

/*@ malloc(a)  = 
    REQ  TRUE
    ENS (∃ r : r=NULL ; malloc(a) ; return() ; r) @*/

/*@ gethostbyaddr(a, b, c)  = 
    REQ  TRUE
    ENS (∃ r : r=NULL ; gethostbyaddr(a) ; (!_(r))^* ; r) @*/

/*@ pthread_mutex_lock(a)  = 
    REQ  TRUE
    ENS (∃ r : !(r=0) ; pthread_mutex_lock(a) ; (!_(a))^* ; r) @*/

    
/*@ pthread_mutex_unlock(a)  = 
    REQ  TRUE
    ENS (∃ r : TRUE ; pthread_mutex_unlock(a) ; (_)^* ; r) @*/


/*@ strncpy(a, b, c)  = 
    REQ  TRUE
    ENS (∃ r : TRUE ; strncpy(b) ; (_)^* ; r) @*/

/*@ strcpy(a, b)  = 
    REQ  TRUE
    ENS (∃ r : TRUE ; strcpy(b) ; (_)^* ; r) @*/


/*@ memcpy(a, b, C)  = 
    REQ  TRUE
    ENS (∃ r : TRUE ; memcpy(a, b) ; (_)^* ; r) @*/


/*@ return(t) =
    REQ  TRUE
    ENS (: TRUE ; return() ; (_)^* ; t; -1) @*/

