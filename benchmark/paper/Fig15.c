#include <fcntl.h>    
#include <unistd.h>   
#include <stdlib.h>   

/*@ current_memory_use() =
    REQ  TRUE
    ENS (∃ r1 r2 : r2>=r1 ; CMU(r1);  ((_)^* · CMU(r2) ·  (_)^*) \/ (!CMU(_))^* ; r1) @*/

int current_memory_use (); 

void test() {
    int a = current_memory_use (); 
    int b = current_memory_use (); 

}
