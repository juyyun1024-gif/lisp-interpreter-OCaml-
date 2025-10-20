# Lisp Interpreter — CSCI 2041 Lab 12 & Project 3
**University of Minnesota – Advanced Programming Principles (Spring 2025)**  
*Author: Justin Yun*

## Overview
This project implements a **complete Lisp interpreter in OCaml**, integrating three main components:
1. **Parser** – reads Lisp expressions from text files and converts them into internal OCaml representations (`thing` values).  
2. **Evaluator** – executes Lisp expressions using an environment of symbols, closures, and primitives.  
3. **Printer** – converts internal `thing` values back to Lisp notation for display.
Together, these modules form a working Lisp system capable of reading, evaluating, and printing symbolic Lisp code such as the *equation solver* in `solve.txt`.

## Theory
OCaml provides both an **interactive interpreter** (`ocaml`) and two **compilers**:
- `ocamlc` → bytecode (similar to Java’s `javac` + `java`)  
- `ocamlopt` → native machine code (runs faster; used here)
Running `ocamlopt p.ml` compiles `p.ml` to `a.out`, which you then execute with `./a.out`.

Example:
```bash
ocamlopt hello.ml
./a.out
# → Hello, world!
