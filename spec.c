
/*@ free(ptr)  = 
    REQ  ∃loc · ptr=loc  
    ENS ( : r=unit ; free(loc) ; (!_(loc))^* ; r) @*/

/*@ malloc(size) = 
    REQ size > 0 
    ENS (loc : r=loc ; malloc(loc)  ; (!free(loc))^* · free(loc) ·  (_)^* ; r)  
@*/

