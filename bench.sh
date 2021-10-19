#!/bin/sh
hyperfine "dune exec ./src/rustscript.exe \
            'let fib = fn(n) => if n < 1 then 1 else fib(n - 1) + fib(n - 2)'\
            'fib(30)'"
