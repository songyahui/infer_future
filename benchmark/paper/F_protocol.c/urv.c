// https://cwe.mitre.org/data/definitions/252.html

#include <stdio.h>
#include <unistd.h>

#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>

void test1 () {
    char buf[10], cp_buf[10];
    fgets(buf, 10, stdin);
    strcpy(cp_buf, buf);
}



int main() {
    char destBuf[10], srcBuf[10];
    memcpy(destBuf, srcBuf, (returnChunkSize(destBuf)-1));

}

int example3() {
    char* buf, xfer; 
    int req_size = 4; 
    buf = (char*) malloc(req_size);
    strncpy(buf, xfer, req_size);
}

struct hostent
{
    int h_name; 
};


void host_lookup(char *user_supplied_addr){
    struct hostent *hp;
    void *addr;
    char hostname[64];
    // in_addr_t inet_addr(const char *cp);
    
    /*routine that ensures user_supplied_addr is in the right format for conversion */
    
    validate_addr_form(user_supplied_addr);
    addr = inet_addr(user_supplied_addr);
    hp = gethostbyaddr( addr, 8, 9);
    strcpy(hostname, hp->h_name);
    }




int f() {
    int result;
    pthread_mutex_t mutex;

    result = pthread_mutex_lock(&mutex);
    // if (0 != result) return result;


    /* access shared resource */


    return pthread_mutex_unlock(&mutex);
}