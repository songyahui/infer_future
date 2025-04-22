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