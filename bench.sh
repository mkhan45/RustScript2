#!/bin/sh
hyperfine --warmup 3 "dune exec ./bin/rustscript_cli.exe examples/fib.rsc"
