
infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/'


 ../../infer-osx-x86_64-v1.2.0/bin/infer run -- clang -c 

# File

```
infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/A_file/recouce_leak.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/A_file/Fig2.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/A_file/Fig5.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/A_file/Fig16.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/A_file/test_fatfs_common.c'
```


# Thread 

```
infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/B_thread/zombie1.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/B_thread/Zombie.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/B_thread/deadlock.c'
```


# Memory Benchmark 

```

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/double_free.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/CWE-416-Use-After-Free.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/Fig4.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/Fig5.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/memoery-loop.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/memory_leak.c'

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/uaf.c'
```




```

 ../../infer-osx-x86_64-v1.2.0/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/double_free.c'

 ../../infer-osx-x86_64-v1.2.0/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/CWE-416-Use-After-Free.c'

 ../../infer-osx-x86_64-v1.2.0/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/Fig4.c'

 ../../infer-osx-x86_64-v1.2.0/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/Fig5.c'

 ../../infer-osx-x86_64-v1.2.0/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/memoery-loop.c'

 ../../infer-osx-x86_64-v1.2.0/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/memory_leak.c'

 ../../infer-osx-x86_64-v1.2.0/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/C_memory/uaf.c'
```


# Socket # Database # UCR 
```
infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/D_socket/socket.c' 

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/E_database/database.c' 

infer/bin/infer run -- clang -c '/Users/yahuis/Desktop/git/infer_future/benchmark/paper/F_protocol.c/Fig17.c' 
```








I am trying to build a termination analyzer for large projects


https://gitlab.com/sosy-lab/benchmarking/sv-benchmarks/-/tree/main/c?ref_type=heads
https://sv-comp.sosy-lab.org/2024/benchmarks.php


./infer/bin/infer run --pulse-only -- clang++ -c benchmark/pulseinf/loop_conditional_non_terminate.cpp

./infer/bin/infer run --pulse-only -- make --keep-going 
./build-infer.sh clang      