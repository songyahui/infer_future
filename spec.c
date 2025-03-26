
/*@ free(ptr)  = 
    REQ  TRUE
    ENS (r : r=unit ; free(ptr) ; (!_(ptr))^* ; r) @*/

/*@ malloc(size) = 
    REQ size > 0 
    ENS (loc : TRUE ; malloc(loc)  ; (!free(loc))^* · free(loc) ·  (_)^* ; loc)  
@*/

/*@ open(path, flag)  = 
    REQ TRUE
    ENS ( : r=-1 ; 𝝐 ; exit(-1) · (_)^* ; r)  
     \/ (fd  : !(r=-1) ∧ flag=0 ; open(fd) ; (!write(fd))^* /\ (!close(fd))^* · close(fd) ·  (_)^* ; fd) 
     \/ (fd : !(r=-1) ∧ flag=1 ; open(fd) ; (!read(fd))^* /\ (!close(fd))^* · close(fd) ·  (_)^* ; fd) 
     \/ (fd : !(r=-1) ∧ flag=2 ; open(fd) ; (!close(fd))^* · close(fd) ·  (_)^* ; fd) 
@*/

/*@ close(fd) = 
    REQ  TRUE
    ENS (r : r=unit ; close(fd) ; (!_(fd))^* ; r) @*/

/*@ write(fd, buf, size) =
    REQ  TRUE
    ENS (r : !(r=-1) ; write(fd) ;  (_)^* ; r) 
    \/ (r : r=-1 ; 𝝐 ; close(fd) · exit(-1) · (_)^* ; r)  @*/

/*@ read(fd, buf, size) =
    REQ  TRUE
    ENS (r : TRUE ; read(fd) ; (_)^* ; r) @*/

/*@ exit(code) =
    REQ  TRUE
    ENS (r : r=code ; exit(code) ; (_)^* ; r; -2 ) @*/

/*@ return(t) =
    REQ  TRUE
    ENS ( : TRUE ; 𝝐 ; (_)^* ; t; -1) @*/
