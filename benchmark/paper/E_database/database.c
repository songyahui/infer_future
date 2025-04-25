// https://developernote.com/2019/09/measuring-sqlite-performance/


#include <sqlite3.h>
#include <stdio.h>
#include <unistd.h>

/*@ sqlite3_open(path, db)  = 
    REQ  TRUE
    ENS (∃ r : !(r=0) ; sqlite3_open(ptr) ; (!sqlite3_close(l))^* · sqlite3_close(l) ·  (_)^* ; r) @*/

/*@ sqlite3_close(db)  = 
    REQ  TRUE
    ENS (∃ r : r=unit ; sqlite3_close(db) ; (!_(db))^* ; r) @*/

 
static int callback(void *NotUsed, int argc, char **argv, char **azColName)
{
    return 0;
}
 
int main()
{
    sqlite3 *db;
    int rc;
    rc = sqlite3_open("familyGuy.db", &db);
    if (rc) {
        //std::cout << "Can't open database: " << sqlite3_errmsg(db) << "\n";
        return 1;
    } else {
        //std::cout << "Open database successfully\n\n";
    }
    

    // ... perform benchmark operations ...

    // sqlite3_close(db);
    return 0;
}

int main1() {
    
        sqlite3 *db;
        int rc = sqlite3_open("test.db", &db);
        if (rc != SQLITE_OK) {
            fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
            sqlite3_close(db);  // Even if open fails, db may need closing
            return 1;
        }
        // Do something with db...
        // Forgot sqlite3_close(db) - LEAK!
    }

void query_database() {
    sqlite3 *db;
    if (sqlite3_open("test.db", &db) != SQLITE_OK) {
        fprintf(stderr, "Failed to open DB\n");
        return;  // Leak: db is not closed
    }

    char *err_msg = 0;
    if (sqlite3_exec(db, "SELECT * FROM users;", 0, 0, &err_msg) != SQLITE_OK) {
        fprintf(stderr, "SQL error: %s\n", err_msg);
        sqlite3_free(err_msg);
        return;  // Leak: db is not closed
    }

    sqlite3_close(db);  // Only closes if everything succeeds
}

int main2() {
    sqlite3 *db;
    if (sqlite3_open("test1.db", &db) != SQLITE_OK) {
        fprintf(stderr, "Failed to open DB1\n");
        return 1;
    }

    // Later in code...
    if (sqlite3_open("test2.db", &db) != SQLITE_OK) {
        fprintf(stderr, "Failed to open DB2\n");
        return 1;
    }  // Leak: test1.db is never closed!

    sqlite3_close(db);  // Only closes test2.db
    return 0;
}

int main3() {
    sqlite3 *db;
    if (sqlite3_open("test.db", &db) != SQLITE_OK) {
        fprintf(stderr, "Failed to open DB\n");
        return 1;
    }

    pid_t pid = fork();
    if (pid == 0) {
        // Child process
        printf("Child process with db=%p\n", db);
        _exit(0);  // Leak: Child does not close db
    } else {
        // Parent process
        printf("Parent process with db=%p\n", db);
        sqlite3_close(db);  // Only parent closes
    }

    return 0;
}