# Build from Docker

For artifact evaluation, we have built a docker image, which (is recommended and) can be accessed
by the following commands:

```
docker pull anonymous614/sas25-future-condition:default
docker run -i -t anonymous614/sas25-future-condition:default /bin/bash
```


# Compile  

```
cd home/infer_future
./compile
```

# Reproduce Table 3 

```
./table3 
```

# Project Structure (Main files)

- Main Logic for forward reasoning: `infer_future/infer/src/clang/cFrontend.ml` 
- Utility functions and data structure definition: `infer_future/infer/src/clang/ast_utility.ml`
- Parser: `infer_future/infer/src/clang/parser.mly` 
- Lexer: `infer_future/infer/src/clang/lexer.mll` 

# Experimental Results Reproduce 

## 1_File

```
infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/A_file/recouce_leak.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/A_file/Fig2.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/A_file/Fig5.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/A_file/Fig16.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/A1_file/test_fatfs_common.c'
```


## 2_Thread 

```
infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/B_thread/zombie1.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/B_thread/Zombie.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/B_thread/deadlock.c'
```


## 3_Memory Benchmark 

```

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/double_free.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/CWE-416-Use-After-Free.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/Fig4.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/Fig5.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/memoery-loop.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/memory_leak.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/uaf.c'
```


## 4_Socket, 5_Database,  6_UCR_NPD
```
infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/D_socket/socket.c' 

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/E_database/database.c' 

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/F_protocol.c/Fig17.c' 
```

