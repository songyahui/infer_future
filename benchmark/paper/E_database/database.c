// https://developernote.com/2019/09/measuring-sqlite-performance/

#include <iostream>
#include "sqlite3.h"

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

    sqlite3_close(db);
    return 0;
}