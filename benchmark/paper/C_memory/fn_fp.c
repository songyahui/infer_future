#include <stdlib.h>


int main () {
    int* ptr2 = malloc (4);
    {
        int ptr1 = malloc (4);   // free (ptr1)
        ptr2 = &ptr1;
        // ptr1->_ * ptr2->ptr1    
    }
    free(*ptr2);
    free(ptr2);
    // ex p. p->_ * ptr2->p

    return 0; 
    
}