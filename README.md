# Build from Docker

For artifact evaluation, we have built a docker image, which (is recommended and) can be accessed
by the following commands:

```
docker pull anonymous614/sas25-future-condition:default
docker run -i -t anonymous614/sas25-future-condition:default /bin/bash
```

# Reproduce Table 3 (Validated badge)

To reproduce the experimental results from the paper:
Navigate to the source code directory.
Run the script to generate the complete Table 3.
This process typically takes 3–5 minutes, and the console will display the intermediate results and inferred specifications.

```
cd home/infer_future
./table3 
```

By the end of the execution, you will see a table like the one below.
This table includes more lines than the version in the paper because it lists all analysis files individually, whereas the paper groups results by category.
The files are organized by folder -- for example, `benchmark/paper/A_file` and `benchmark/paper/F_protocol.c`.


```
+----------------------------------------------------------------------+------+-------+-----------+-------------+--------+-----------------+
|                               Category                               | LoC  | PrimS | InferredS | InferredInv | Report |     Time(s)     |
+======================================================================+======+=======+===========+=============+========+=================+
| /home/infer_future/benchmark/paper/A_file/recouce_leak.c             | 23   | 8     | 1         | 1           | 1      | 0.756446123123  |
+----------------------------------------------------------------------+------+-------+-----------+-------------+--------+-----------------+
                                                  ......................................
+----------------------------------------------------------------------+------+-------+-----------+-------------+--------+-----------------+
| /home/infer_future/benchmark/paper/F_protocol.c/Fig17.c              | 95   | 15    | 4         | 0           | 4      | 0.0258920192719 |
+----------------------------------------------------------------------+------+-------+-----------+-------------+--------+-----------------+
| -                                                                    | 1721 | -     | 101       | 21          | 55     | 19.214118719    |
+----------------------------------------------------------------------+------+-------+-----------+-------------+--------+-----------------+
```

The numbers in the last line do not exactly match those in the paper due to minor inaccuracies introduced during manual data collection. We will correct these figures in the next version of the paper.

# Compile (Extensible badge)

## To compile from the docker: Navigate to the source code directory. Run the compile script. The compilation may take 3-5 mins. 

```
cd home/infer_future
./compile
```

## To compile from the source code: 


```
# Dependencies: 
apt-get update
apt-get upgrade
apt install opam
opam install z3
apt install menhir
apt install cmake
apt install sqlite3 
apt install curl
# Compile from the source: 
cd infer_future # navigate to the folder 
./compile
```

## To facilitate the extension, here presents the main files and their functionalities: 

- Main Logic for forward reasoning: `infer_future/infer/src/clang/cFrontend.ml`. The forward rules are in the function "forward_reasoning"; and before that the main logic is defined in "do_source_file" function. 
- Utility functions and data structure definition: `infer_future/infer/src/clang/ast_utility.ml`. The main data type for the specification is named by "summary", and future conditions is defined as "futureCond". 
- Parser: `infer_future/infer/src/clang/parser.mly`. The current entry for the parser is "summary" and "standaloneFC" for parsing the user defined specification. 
- Lexer: `infer_future/infer/src/clang/lexer.mll` 
- Test Cases: `benchmark/paper/`. Each subfolder in this directory contains a list of programs which can be verified. Each subfolder also contains one file named `spec.c`, containing all the predefined specifications.  


