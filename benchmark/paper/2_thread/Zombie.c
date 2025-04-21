
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>

void* thread_func(void* arg) {
    printf("Hello from thread!\n");
    return NULL;
}

int main() {
    pthread_t t;
    pthread_create(&t, NULL, thread_func, NULL);
    pthread_join(t, NULL); // Properly joins the thread
    return 0;
}

pthread_mutex_t lock;

void* thread_func1(void* arg) {
    pthread_mutex_lock(&lock);
    printf("Thread acquired lock\n");
    pthread_mutex_unlock(&lock);
    return NULL;
}

int main1() {
    pthread_t t;
    pthread_mutex_init(&lock, NULL);
    pthread_create(&t, NULL, thread_func1, NULL);
    pthread_join(t, NULL); // Properly joins the thread
    pthread_mutex_destroy(&lock);
    return 0;
}


void* say_hello(void* arg) {
    printf("Hello from thread %ld\n", (long)arg);
    return NULL;
}

int main_Zombie1() {
    pthread_t t1, t2;
    pthread_create(&t1, NULL, say_hello, (void*)1);
    pthread_create(&t2, NULL, say_hello, (void*)2);

    // Main thread does NOT join or detach t1 or t2
    sleep(1); // Give threads time to finish
    printf("Main thread exiting without joining or detaching threads.\n");
    return 0;
}




int main_Zombie2() {
    pthread_t tid;
    pthread_create(&tid, NULL, thread_func, NULL);
    // Main thread exits before joining the child thread
    // The child thread is left in a zombie state if it finishes after main
    return 0;
}
