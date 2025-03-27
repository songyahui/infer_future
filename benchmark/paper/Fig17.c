#include <fcntl.h>    // For open()
#include <unistd.h>   // For write(), close()
#include <stdlib.h>   // For exit()


/*@ start_doc() =
    REQ  TRUE
    ENS (: TRUE ; start_doc(); (start_page() · (!end_doc())^* · end_page() )^* · end_doc() ; unit) @*/

/*@ start_page() =
    REQ  TRUE
    ENS (: TRUE ; start_page(); (draw())^* · end_page() · (_)^* ; unit) @*/

/*@ draw() =
    REQ  TRUE
    ENS (: TRUE ; draw();  (_)^* ; unit) @*/

/*@ end_page() =
    REQ  TRUE
    ENS (: TRUE ; end_page();  (_)^* ; unit) @*/

/*@ end_doc() =
    REQ  TRUE
    ENS (: TRUE ; end_doc();  (_)^* ; unit) @*/

void start_doc(); 
void start_page(); 
void draw();     
void end_page(); 
void end_doc();  

void testPainting () 
{ 
    start_doc(); 
    start_page(); 
    draw();     
    draw();     
    draw();     
    end_page(); 
    end_doc();  
}


void testPainting1 () 
{ 
    start_doc(); 
    start_page(); 
    draw();     
    draw();     
    draw();     
    end_page(); 

    start_page(); 
    draw();     
    draw();     
    end_page(); 

    end_doc();  
}


void testPainting2 () 
{ 
    start_doc(); 
    start_page(); 
    draw();     
    draw();     
    draw();     
    end_page(); 

    start_page(); 
    draw();     
    draw();     
    end_page(); 

    end_doc();  
}


void testPainting3 () 
{ 
    start_doc(); 
    start_page(); 
    draw();     
    draw();     
    draw();     
    end_page(); 


    draw();     
    draw();     
    end_page(); 

    end_doc();  
}
