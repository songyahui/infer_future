#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>

pthread_mutex_t resource1, resource2;

void* proc1(void* arg) {
    printf("proc1 locking resource1\n");
    pthread_mutex_lock(&resource1);
    usleep(200); // simulate work
    printf("proc1 trying to lock resource2\n");
    pthread_mutex_lock(&resource2);
    printf("proc1 acquired resource2\n");
    pthread_mutex_unlock(&resource2);
    pthread_mutex_unlock(&resource1);
    return NULL;
}

void* proc2(void* arg) {
    printf("proc2 locking resource2\n");
    pthread_mutex_lock(&resource2);
    usleep(200); // simulate work
    printf("proc2 trying to lock resource1\n");
    pthread_mutex_lock(&resource1);
    printf("proc2 acquired resource1\n");
    pthread_mutex_unlock(&resource1);
    pthread_mutex_unlock(&resource2);
    return NULL;
}

int main() {
    pthread_t t1, t2;
    pthread_mutex_init(&resource1, NULL);
    pthread_mutex_init(&resource2, NULL);

    pthread_create(&t1, NULL, proc1, NULL);
    pthread_create(&t2, NULL, proc2, NULL);

    pthread_join(t1, NULL); // Will never return if deadlock occurs
    pthread_join(t2, NULL);

    pthread_mutex_destroy(&resource1);
    pthread_mutex_destroy(&resource2);
    return 0;
}



// Both threads will lock resource1, then resource2
void* proc11(void* arg) {
    printf("proc1 locking resource1\n");
    pthread_mutex_lock(&resource1);
    usleep(200);
    printf("proc1 locking resource2\n");
    pthread_mutex_lock(&resource2);

    printf("proc1 acquired both resources\n");

    pthread_mutex_unlock(&resource2);
    pthread_mutex_unlock(&resource1);
    return NULL;
}

void* proc12(void* arg) {
    printf("proc2 locking resource1\n");
    pthread_mutex_lock(&resource1);
    usleep(200);
    printf("proc2 locking resource2\n");
    pthread_mutex_lock(&resource2);

    printf("proc2 acquired both resources\n");

    pthread_mutex_unlock(&resource2);
    pthread_mutex_unlock(&resource1);
    return NULL;
}

int main1() {
    pthread_t t1, t2;
    pthread_mutex_init(&resource1, NULL);
    pthread_mutex_init(&resource2, NULL);

    pthread_create(&t1, NULL, proc1, NULL);
    pthread_create(&t2, NULL, proc2, NULL);

    pthread_join(t1, NULL);
    pthread_join(t2, NULL);

    pthread_mutex_destroy(&resource1);
    pthread_mutex_destroy(&resource2);
    return 0;
}
