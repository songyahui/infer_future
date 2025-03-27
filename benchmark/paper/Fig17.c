#include <fcntl.h>    // For open()
#include <unistd.h>   // For write(), close()
#include <stdlib.h>   // For exit()

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


    draw();     
    draw();     
    end_page(); 

    end_doc();  
}
