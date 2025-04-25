#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

void create_leaky_connection(int port) {
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
        perror("socket");
        return;
    }

    struct sockaddr_in server_addr = {
        .sin_family = AF_INET,
        .sin_port = htons(port),
        .sin_addr.s_addr = INADDR_ANY
    };

    if (connect(sock, (struct sockaddr*)&server_addr, sizeof(server_addr))) {
        perror("connect");
        // Intentionally not closing sock here to simulate leak
        return;
    }
    // close (sock);
    // Normally should close(sock) here, but intentionally omitted
}

int main() {
    for (int i = 0; i < 1000; i++) {
        create_leaky_connection(8080);
        // Monitor system resources here
    }
    return 0;
}

int main1() {
    int sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) {
        perror("socket failed");
        return 1;
    }

    // Later in the code...
    sockfd = socket(AF_INET, SOCK_STREAM, 0); // Overwrites old sockfd without closing
    if (sockfd < 0) {
        perror("second socket failed");
        return 1;
    }

    close(sockfd); // Only closes the second socket
    return 0;
}

int main3() {
    int sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) {
        perror("socket failed");
        return 1;
    }

    pid_t pid = fork();
    if (pid == 0) {
        // Child process
        printf("Child process with sockfd=%d\n", sockfd);
        // Does not close sockfd - LEAK!
        _exit(0);
    } else {
        // Parent process
        printf("Parent process with sockfd=%d\n", sockfd);
        close(sockfd); // Only parent closes
    }

    return 0;
}

void safe_connect() {
    int sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) {
        perror("socket failed");
        return;
    }

    struct sockaddr_in server_addr = {0};
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(8080);
    server_addr.sin_addr.s_addr = inet_addr("127.0.0.1");

    if (connect(sockfd, (struct sockaddr*)&server_addr, sizeof(server_addr)) < 0) {
        perror("connect failed");
        close(sockfd); // Close on error
        return;
    }

    // Do something with the socket
    close(sockfd); // Close on success
}

int main4() {
    safe_connect();
    return 0;
}