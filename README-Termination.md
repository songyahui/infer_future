I am trying to build a termination analyzer for large projects


https://gitlab.com/sosy-lab/benchmarking/sv-benchmarks/-/tree/main/c?ref_type=heads
https://sv-comp.sosy-lab.org/2024/benchmarks.php


./infer/bin/infer run --pulse-only -- clang++ -c benchmark/pulseinf/loop_conditional_non_terminate.cpp

./infer/bin/infer run --pulse-only -- make --keep-going 
./build-infer.sh clang      