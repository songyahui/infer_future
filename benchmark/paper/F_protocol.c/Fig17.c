#include <fcntl.h>    // For open()
#include <unistd.h>   // For write(), close()
#include <stdlib.h>   // For exit()

/*@ start_doc(a) =
    REQ  TRUE
    ENS (: TRUE ; start_doc(a); (start_page(a) · (!end_doc(a))^* · end_page(a) )^* · end_doc(a) ; unit) @*/

/*@ start_page(a) =
    REQ  TRUE
    ENS (: TRUE ; start_page(a); (draw())^* · end_page(a) · (!_(a))^* ; unit) @*/

/*@ draw(a) =
    REQ  TRUE
    ENS (: TRUE ; draw();  (_)^* ; unit) @*/

/*@ end_page(a) =
    REQ  TRUE
    ENS (: TRUE ; end_page(a);  (_)^* ; unit) @*/

/*@ end_doc(a) =
    REQ  TRUE
    ENS (: TRUE ; end_doc(a);  (_)^* ; unit) @*/

void start_doc(int a); 
void start_page(int a); 
void draw(int a); 
void end_page(int a); 
void end_doc(int a); 

void testPainting () 
{ 
    start_doc(1); 
    start_page(1); 
    draw(1);     
    draw(1);     
    draw(1);     
    end_page(1); 
    end_doc(1);  
}


void testPainting1 () 
{ 
    start_doc(1); 
    start_page(1); 
    draw(1);     
    draw(1);     
    draw(1);     
    end_page(1); 

    start_page(2); 
    draw(2);     
    draw(2);     
    end_page(2); 

    end_doc(1);  
}


void testPainting2 () 
{ 
    start_doc(1); 
    start_page(1); 
    draw(1);     
    draw(1);     
    draw(1);     
    end_page(1); 

    start_page(2); 
    draw(2);     
    draw(2);     
    end_page(2); 

    end_doc(1);  
}


void testPainting3 () 
{ 
    start_doc(1); 
    start_page(1); 
    draw(1);     
    draw(1);     
    draw(1);     
    end_page(1); 


    draw(1);     
    draw(1);     
    end_page(1); 

    end_doc(1);  
}
