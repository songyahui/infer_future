/*
 * SPDX-FileCopyrightText: 2015-2024 Espressif Systems (Shanghai) CO LTD
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <fcntl.h>
#include <sys/time.h>
#include <sys/unistd.h>
#include <sys/stat.h>
#include <errno.h>
#include <utime.h>


const char* fatfs_test_hello_str = "Hello, World!\n";
const char* fatfs_test_hello_str_utf = "世界，你好！\n";

void test_fatfs_create_file_with_text(const char* name, const char* text)
{
    FILE* f = fopen(name, "wb");
    TEST_ASSERT_NOT_NULL(f);
    TEST_ASSERT_TRUE(fputs(text, f) != EOF);
    TEST_ASSERT_EQUAL(0, fclose(f));
}

void test_fatfs_create_file_with_o_creat_flag(const char* filename)
{
    const int fd = open(filename, O_CREAT|O_WRONLY);
    TEST_ASSERT_NOT_EQUAL(-1, fd);

    const int r = pwrite(fd, fatfs_test_hello_str, strlen(fatfs_test_hello_str), 0); //offset=0
    TEST_ASSERT_EQUAL(strlen(fatfs_test_hello_str), r);

    TEST_ASSERT_EQUAL(0, close(fd));
}

void test_fatfs_open_file_with_o_creat_flag(const char* filename)
{
    char buf[32] = { 0 };
    const int fd = open(filename, O_CREAT|O_RDONLY);
    TEST_ASSERT_NOT_EQUAL(-1, fd);

    int r = pread(fd, buf, sizeof(buf), 0); // it is a regular read() with offset==0
    TEST_ASSERT_EQUAL(0, strcmp(fatfs_test_hello_str, buf));
    TEST_ASSERT_EQUAL(strlen(fatfs_test_hello_str), r);

    TEST_ASSERT_EQUAL(0, close(fd));
}

void test_fatfs_overwrite_append(const char* filename)
{
    /* Create new file with 'aaaa' */
    test_fatfs_create_file_with_text(filename, "aaaa");

    /* Append 'bbbb' to file */
    FILE *f_a = fopen(filename, "a");
    TEST_ASSERT_NOT_NULL(f_a);
    TEST_ASSERT_NOT_EQUAL(EOF, fputs("bbbb", f_a));
    TEST_ASSERT_EQUAL(0, fclose(f_a));

    /* Read back 8 bytes from file, verify it's 'aaaabbbb' */
    char buf[10] = { 0 };
    FILE *f_r = fopen(filename, "r");
    TEST_ASSERT_NOT_NULL(f_r);
    TEST_ASSERT_EQUAL(8, fread(buf, 1, 8, f_r));
    TEST_ASSERT_EQUAL_STRING_LEN("aaaabbbb", buf, 8);

    /* Be sure we're at end of file */
    TEST_ASSERT_EQUAL(0, fread(buf, 1, 8, f_r));

    TEST_ASSERT_EQUAL(0, fclose(f_r));

    /* Overwrite file with 'cccc' */
    test_fatfs_create_file_with_text(filename, "cccc");

    /* Verify file now only contains 'cccc' */
    f_r = fopen(filename, "r");
    TEST_ASSERT_NOT_NULL(f_r);
    bzero(buf, sizeof(buf));
    TEST_ASSERT_EQUAL(4, fread(buf, 1, 8, f_r)); // trying to read 8 bytes, only expecting 4
    TEST_ASSERT_EQUAL_STRING_LEN("cccc", buf, 4);
    TEST_ASSERT_EQUAL(0, fclose(f_r));
}

void test_fatfs_read_file(const char* filename)
{
    FILE* f = fopen(filename, "r");
    TEST_ASSERT_NOT_NULL(f);
    char buf[32] = { 0 };
    int cb = fread(buf, 1, sizeof(buf), f);
    TEST_ASSERT_EQUAL(strlen(fatfs_test_hello_str), cb);
    TEST_ASSERT_EQUAL(0, strcmp(fatfs_test_hello_str, buf));
    TEST_ASSERT_EQUAL(0, fclose(f));
}

void test_fatfs_read_file_utf_8(const char* filename)
{
    FILE* f = fopen(filename, "r");
    TEST_ASSERT_NOT_NULL(f);
    char buf[64] = { 0 };   //Doubled buffer size to allow for longer UTF-8 strings
    int cb = fread(buf, 1, sizeof(buf), f);
    TEST_ASSERT_EQUAL(strlen(fatfs_test_hello_str_utf), cb);
    TEST_ASSERT_EQUAL(0, strcmp(fatfs_test_hello_str_utf, buf));
    TEST_ASSERT_EQUAL(0, fclose(f));
}

void test_fatfs_pread_file(const char* filename)
{
    char buf[32] = { 0 };
    const int fd = open(filename, O_RDONLY);
    TEST_ASSERT_NOT_EQUAL(-1, fd);

    int r = pread(fd, buf, sizeof(buf), 0); // it is a regular read() with offset==0
    TEST_ASSERT_EQUAL(0, strcmp(fatfs_test_hello_str, buf));
    TEST_ASSERT_EQUAL(strlen(fatfs_test_hello_str), r);

    memset(buf, 0, sizeof(buf));
    r = pread(fd, buf, sizeof(buf), 1); // offset==1
    TEST_ASSERT_EQUAL(0, strcmp(fatfs_test_hello_str + 1, buf));
    TEST_ASSERT_EQUAL(strlen(fatfs_test_hello_str) - 1, r);

    memset(buf, 0, sizeof(buf));
    r = pread(fd, buf, sizeof(buf), 5); // offset==5
    TEST_ASSERT_EQUAL(0, strcmp(fatfs_test_hello_str + 5, buf));
    TEST_ASSERT_EQUAL(strlen(fatfs_test_hello_str) - 5, r);

    // regular read() should work now because pread() should not affect the current position in file

    memset(buf, 0, sizeof(buf));
    r = read(fd, buf, sizeof(buf)); // note that this is read() and not pread()
    TEST_ASSERT_EQUAL(0, strcmp(fatfs_test_hello_str, buf));
    TEST_ASSERT_EQUAL(strlen(fatfs_test_hello_str), r);

    memset(buf, 0, sizeof(buf));
    r = pread(fd, buf, sizeof(buf), 10); // offset==10
    TEST_ASSERT_EQUAL(0, strcmp(fatfs_test_hello_str + 10, buf));
    TEST_ASSERT_EQUAL(strlen(fatfs_test_hello_str) - 10, r);

    memset(buf, 0, sizeof(buf));
    r = pread(fd, buf, sizeof(buf), strlen(fatfs_test_hello_str) + 1); // offset to EOF
    TEST_ASSERT_EQUAL(0, r);

    TEST_ASSERT_EQUAL(0, close(fd));
}

static void test_pwrite(const char *filename, off_t offset, const char *msg)
{
    const int fd = open(filename, O_WRONLY);
    TEST_ASSERT_NOT_EQUAL(-1, fd);

    const off_t current_pos = lseek(fd, 0, SEEK_END); // O_APPEND is not the same - jumps to the end only before write()

    const int r = pwrite(fd, msg, strlen(msg), offset);
    TEST_ASSERT_EQUAL(strlen(msg), r);

    TEST_ASSERT_EQUAL(current_pos, lseek(fd, 0, SEEK_CUR)); // pwrite should not move the pointer

    TEST_ASSERT_EQUAL(0, close(fd));
}

static void test_file_content(const char *filename, const char *msg)
{
    char buf[32] = { 0 };
    const int fd = open(filename, O_RDONLY);
    TEST_ASSERT_NOT_EQUAL(-1, fd);

    int r = read(fd, buf, sizeof(buf));
    TEST_ASSERT_NOT_EQUAL(-1, r);
    TEST_ASSERT_EQUAL(0, strcmp(msg, buf));

    TEST_ASSERT_EQUAL(0, close(fd));
}

void test_fatfs_pwrite_file(const char *filename)
{
    int fd = open(filename, O_WRONLY | O_CREAT | O_TRUNC);
    TEST_ASSERT_NOT_EQUAL(-1, fd);
    TEST_ASSERT_EQUAL(0, close(fd));

    test_pwrite(filename, 0, "Hello");
    test_file_content(filename, "Hello");

    test_pwrite(filename, strlen("Hello"), ", world!");
    test_file_content(filename, "Hello, world!");
    test_pwrite(filename, strlen("Hello, "), "Dolly");
    test_file_content(filename, "Hello, Dolly!");
}

void test_fatfs_open_max_files(const char* filename_prefix, size_t files_count)
{
    FILE** files = calloc(files_count, sizeof(FILE*));
    for (size_t i = 0; i < files_count; ++i) {
        char name[32];
        snprintf(name, sizeof(name), "%s_%d.txt", filename_prefix, i);
        files[i] = fopen(name, "w");
        TEST_ASSERT_NOT_NULL(files[i]);
    }
    /* close everything and clean up */
    for (size_t i = 0; i < files_count; ++i) {
        fclose(files[i]);
    }
    free(files);
}

void test_fatfs_lseek(const char* filename)
{
    FILE* f = fopen(filename, "wb+");
    TEST_ASSERT_NOT_NULL(f);
    TEST_ASSERT_EQUAL(11, fprintf(f, "0123456789\n"));
    TEST_ASSERT_EQUAL(0, fseek(f, -2, SEEK_CUR));
    TEST_ASSERT_EQUAL('9', fgetc(f));
    TEST_ASSERT_EQUAL(0, fseek(f, 3, SEEK_SET));
    TEST_ASSERT_EQUAL('3', fgetc(f));
    TEST_ASSERT_EQUAL(0, fseek(f, -3, SEEK_END));
    TEST_ASSERT_EQUAL('8', fgetc(f));
    TEST_ASSERT_EQUAL(0, fseek(f, 3, SEEK_END));
    TEST_ASSERT_EQUAL(14, ftell(f));
    TEST_ASSERT_EQUAL(4, fprintf(f, "abc\n"));
    TEST_ASSERT_EQUAL(0, fseek(f, 0, SEEK_END));
    TEST_ASSERT_EQUAL(18, ftell(f));
    TEST_ASSERT_EQUAL(0, fseek(f, 0, SEEK_SET));
    char buf[20];

    TEST_ASSERT_EQUAL(18, fread(buf, 1, sizeof(buf), f));
    const char ref_buf[] = "0123456789\n\0\0\0abc\n";
    TEST_ASSERT_EQUAL_INT8_ARRAY(ref_buf, buf, sizeof(ref_buf) - 1);
    TEST_ASSERT_EQUAL(0, fclose(f));

#ifdef CONFIG_FATFS_USE_FASTSEEK
    f = fopen(filename, "rb+");
    TEST_ASSERT_NOT_NULL(f);
    TEST_ASSERT_EQUAL(0, fseek(f, 0, SEEK_END));
    TEST_ASSERT_EQUAL(18, ftell(f));
    TEST_ASSERT_EQUAL(0, fseek(f, -4, SEEK_CUR));
    TEST_ASSERT_EQUAL(14, ftell(f));
    TEST_ASSERT_EQUAL(0, fseek(f, -14, SEEK_CUR));
    TEST_ASSERT_EQUAL(0, ftell(f));

    TEST_ASSERT_EQUAL(18, fread(buf, 1, sizeof(buf), f));
    TEST_ASSERT_EQUAL_INT8_ARRAY(ref_buf, buf, sizeof(ref_buf) - 1);
    TEST_ASSERT_EQUAL(0, fclose(f));
#endif

}


void test_fatfs_stat(const char* filename, const char* root_dir)
{
    struct tm tm = {
        .tm_year = 2017 - 1900,
        .tm_mon = 11,
        .tm_mday = 8,
        .tm_hour = 19,
        .tm_min = 51,
        .tm_sec = 10
    };
    time_t t = mktime(&tm);
    printf("Setting time: %s", asctime(&tm));
    struct timeval now = { .tv_sec = t };
    settimeofday(&now, NULL);

    test_fatfs_create_file_with_text(filename, "foo\n");

    struct stat st;
    TEST_ASSERT_EQUAL(0, stat(filename, &st));
    time_t mtime = st.st_mtime;
    struct tm mtm;
    localtime_r(&mtime, &mtm);
    printf("File time: %s", asctime(&mtm));
    TEST_ASSERT(llabs(mtime - t) < 2);    // fatfs library stores time with 2 second precision

    TEST_ASSERT(st.st_mode & S_IFREG);
    TEST_ASSERT_FALSE(st.st_mode & S_IFDIR);

    memset(&st, 0, sizeof(st));
    TEST_ASSERT_EQUAL(0, stat(root_dir, &st));
    TEST_ASSERT(st.st_mode & S_IFDIR);
    TEST_ASSERT_FALSE(st.st_mode & S_IFREG);
}

void test_fatfs_size(const char* filename, const char* content) {
    size_t expected_size = strlen(content);

    int fd = open(filename, O_CREAT | O_WRONLY);
    TEST_ASSERT_NOT_EQUAL(-1, fd);

    ssize_t wr = write(fd, content, expected_size);
    TEST_ASSERT_NOT_EQUAL(-1, wr);

    struct stat st;
    TEST_ASSERT_EQUAL(0, stat(filename, &st));
    TEST_ASSERT_EQUAL(wr, st.st_size);

    ssize_t wr2 = pwrite(fd, content, expected_size, expected_size);
    TEST_ASSERT_NOT_EQUAL(-1, wr2);

    TEST_ASSERT_EQUAL(0, stat(filename, &st));
    TEST_ASSERT_EQUAL(wr + wr2, st.st_size);

    TEST_ASSERT_EQUAL(0, ftruncate(fd, wr));

    TEST_ASSERT_EQUAL(0, stat(filename, &st));
    TEST_ASSERT_EQUAL(wr, st.st_size);

    TEST_ASSERT_EQUAL(0, close(fd));

    wr /= 2;

    TEST_ASSERT_EQUAL(0, truncate(filename, wr));

    TEST_ASSERT_EQUAL(0, stat(filename, &st));
    TEST_ASSERT_EQUAL(wr, st.st_size);
}

void test_fatfs_mtime_dst(const char* filename, const char* root_dir)
{
    struct timeval tv = { 1653638041, 0 };
    settimeofday(&tv, NULL);
    setenv("TZ", "MST7MDT,M3.2.0,M11.1.0", 1);
    tzset();

    struct tm tm;
    time_t sys_time = tv.tv_sec;
    localtime_r(&sys_time, &tm);
    printf("Setting time: %s", asctime(&tm));

    test_fatfs_create_file_with_text(filename, "foo\n");

    struct stat st;
    TEST_ASSERT_EQUAL(0, stat(filename, &st));

    time_t mtime = st.st_mtime;
    struct tm mtm;
    localtime_r(&mtime, &mtm);
    printf("File time: %s", asctime(&mtm));

    TEST_ASSERT(llabs(mtime - sys_time) < 2);    // fatfs library stores time with 2 second precision

    unsetenv("TZ");
    tzset();
}

void test_fatfs_utime(const char* filename, const char* root_dir)
{
    struct stat achieved_stat;
    struct tm desired_tm;
    struct utimbuf desired_time = {
        .actime = 0, // access time is not supported
        .modtime = 0,
    };
    time_t false_now = 0;
    memset(&desired_tm, 0, sizeof(struct tm));

    {
        // Setting up a false actual time - used when the file is created and for modification with the current time
        desired_tm.tm_mon = 10 - 1;
        desired_tm.tm_mday = 31;
        desired_tm.tm_year = 2018 - 1900;
        desired_tm.tm_hour = 10;
        desired_tm.tm_min = 35;
        desired_tm.tm_sec = 23;

        false_now = mktime(&desired_tm);

        struct timeval now = { .tv_sec = false_now };
        settimeofday(&now, NULL);
    }
    test_fatfs_create_file_with_text(filename, "");

    // 00:00:00. January 1st, 1980 - FATFS cannot handle earlier dates
    desired_tm.tm_mon = 1 - 1;
    desired_tm.tm_mday = 1;
    desired_tm.tm_year = 1980 - 1900;
    desired_tm.tm_hour = 0;
    desired_tm.tm_min = 0;
    desired_tm.tm_sec = 0;
    printf("Testing mod. time: %s", asctime(&desired_tm));
    desired_time.modtime = mktime(&desired_tm);
    TEST_ASSERT_EQUAL(0, utime(filename, &desired_time));
    TEST_ASSERT_EQUAL(0, stat(filename, &achieved_stat));
    TEST_ASSERT_EQUAL_UINT32(desired_time.modtime, achieved_stat.st_mtime);

    // current time
    TEST_ASSERT_EQUAL(0, utime(filename, NULL));
    TEST_ASSERT_EQUAL(0, stat(filename, &achieved_stat));
    printf("Mod. time changed to (false actual time): %s", ctime(&achieved_stat.st_mtime));
    TEST_ASSERT_NOT_EQUAL(desired_time.modtime, achieved_stat.st_mtime);
    TEST_ASSERT(false_now - achieved_stat.st_mtime <= 2); // two seconds of tolerance are given

    // 23:59:08. December 31st, 2037
    desired_tm.tm_mon = 12 - 1;
    desired_tm.tm_mday = 31;
    desired_tm.tm_year = 2037 - 1900;
    desired_tm.tm_hour = 23;
    desired_tm.tm_min = 59;
    desired_tm.tm_sec = 8;
    printf("Testing mod. time: %s", asctime(&desired_tm));
    desired_time.modtime = mktime(&desired_tm);
    TEST_ASSERT_EQUAL(0, utime(filename, &desired_time));
    TEST_ASSERT_EQUAL(0, stat(filename, &achieved_stat));
    TEST_ASSERT_EQUAL_UINT32(desired_time.modtime, achieved_stat.st_mtime);

    //WARNING: it has the Unix Millennium bug (Y2K38)

    // 00:00:00. January 1st, 1970 - FATFS cannot handle years before 1980
    desired_tm.tm_mon = 1 - 1;
    desired_tm.tm_mday = 1;
    desired_tm.tm_year = 1970 - 1900;
    desired_tm.tm_hour = 0;
    desired_tm.tm_min = 0;
    desired_tm.tm_sec = 0;
    printf("Testing mod. time: %s", asctime(&desired_tm));
    desired_time.modtime = mktime(&desired_tm);
    TEST_ASSERT_EQUAL(-1, utime(filename, &desired_time));
    TEST_ASSERT_EQUAL(EINVAL, errno);
}

void test_fatfs_unlink(const char* filename)
{
    test_fatfs_create_file_with_text(filename, "unlink\n");

    TEST_ASSERT_EQUAL(0, unlink(filename));

    TEST_ASSERT_NULL(fopen(filename, "r"));
}

void test_fatfs_link_rename(const char* filename_prefix)
{
    char name_copy[64];
    char name_dst[64];
    char name_src[64];
    snprintf(name_copy, sizeof(name_copy), "%s_cpy.txt", filename_prefix);
    snprintf(name_dst, sizeof(name_dst), "%s_dst.txt", filename_prefix);
    snprintf(name_src, sizeof(name_src), "%s_src.txt", filename_prefix);

    unlink(name_copy);
    unlink(name_dst);
    unlink(name_src);

    FILE* f = fopen(name_src, "w+");
    TEST_ASSERT_NOT_NULL(f);
    const char* str = "0123456789";
    for (int i = 0; i < 4000; ++i) {
        TEST_ASSERT_NOT_EQUAL(EOF, fputs(str, f));
    }
    TEST_ASSERT_EQUAL(0, fclose(f));
    TEST_ASSERT_EQUAL(0, link(name_src, name_copy));
    FILE* fcopy = fopen(name_copy, "r");
    TEST_ASSERT_NOT_NULL(fcopy);
    TEST_ASSERT_EQUAL(0, fseek(fcopy, 0, SEEK_END));
    TEST_ASSERT_EQUAL(40000, ftell(fcopy));
    TEST_ASSERT_EQUAL(0, fclose(fcopy));
    TEST_ASSERT_EQUAL(0, rename(name_copy, name_dst));
    TEST_ASSERT_NULL(fopen(name_copy, "r"));
    FILE* fdst = fopen(name_dst, "r");
    TEST_ASSERT_NOT_NULL(fdst);
    TEST_ASSERT_EQUAL(0, fseek(fdst, 0, SEEK_END));
    TEST_ASSERT_EQUAL(40000, ftell(fdst));
    TEST_ASSERT_EQUAL(0, fclose(fdst));
}

void test_fatfs_mkdir_rmdir(const char* filename_prefix)
{
    char name_dir1[64];
    char name_dir2[64];
    char name_dir2_file[64];
    snprintf(name_dir1, sizeof(name_dir1), "%s1", filename_prefix);
    snprintf(name_dir2, sizeof(name_dir2), "%s2", filename_prefix);
    snprintf(name_dir2_file, sizeof(name_dir2_file), "%s2/1.txt", filename_prefix);

    TEST_ASSERT_EQUAL(0, mkdir(name_dir1, 0755));
    struct stat st;
    TEST_ASSERT_EQUAL(0, stat(name_dir1, &st));
    TEST_ASSERT_TRUE(st.st_mode & S_IFDIR);
    TEST_ASSERT_FALSE(st.st_mode & S_IFREG);
    TEST_ASSERT_EQUAL(0, rmdir(name_dir1));
    TEST_ASSERT_EQUAL(-1, stat(name_dir1, &st));

    TEST_ASSERT_EQUAL(0, mkdir(name_dir2, 0755));
    test_fatfs_create_file_with_text(name_dir2_file, "foo\n");
    TEST_ASSERT_EQUAL(0, stat(name_dir2, &st));
    TEST_ASSERT_TRUE(st.st_mode & S_IFDIR);
    TEST_ASSERT_FALSE(st.st_mode & S_IFREG);
    TEST_ASSERT_EQUAL(0, stat(name_dir2_file, &st));
    TEST_ASSERT_FALSE(st.st_mode & S_IFDIR);
    TEST_ASSERT_TRUE(st.st_mode & S_IFREG);
    TEST_ASSERT_EQUAL(-1, rmdir(name_dir2));
    TEST_ASSERT_EQUAL(0, unlink(name_dir2_file));
    TEST_ASSERT_EQUAL(0, rmdir(name_dir2));
}

