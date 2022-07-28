<div align=center>
    <img src="assets/logo.png" width="600px">
    <h1>
        RustScript V2
    </h1>
</div>


V2 of <https://github.com/mkhan45/RustScript>

I wrote RustScript originally in Java because it was part of a school project,
ignoring performance/code quality because I only had one night to do it.

This is an improved version of RustScript with improved performance and more features
written to learn OCaml. It also served as a testbed for features, and a demonstration
of the 80/20 rule; the language's design was largely based on ease of implementation.

### Examples:

The most impressive examples are:
- The tic-tac-toe AI: <https://github.com/mkhan45/RustScript2/blob/main/examples/tictactoe_minimax.rsc>
- The TOML parser library: <https://github.com/mkhan45/rustscript_toml>
- A simple static site generator/HTML template engine: <https://github.com/mkhan45/ssg_rustscript>
- My personal website served using the previous two libraries: <https://github.com/mkhan45/rustscript_site>, hosted at <https://rustscript.mikail-khan.com>
- More examples below

### Language Tour

#### Basic types:

RustScript has 5 basic types
```ex
let x = 5 # integer
let f = 5.0 # float
let s = "Hello" # string
let b = T # boolean
let a = :atom # atom
```

#### Compound types:

There are also a few compound types
```ex
let t = (1, "hello", :aaa) # tuples
let ls = [1, 2, 3, 4, 5] # lists
let m = %{one: 2, "three" => 3} # maps
let f1 = fn(x) => x * 2 # closures
let f2(x) = x * 2 # functions
```

#### Patterns:

All bindings in RustScript are done through pattern matching. Aside from the primitives, there
are:

```ex
let (a, (b, c), d) = (1, (2, 3), 4) # tuple patterns
inspect((a, b, c, d)) # (1, 2, 3, 4)

let [a, b, c] = [1, 2, 3] # list patterns
inspect((a, b, c)) # (1, 2, 3)

let [a, b | tl] = [1, 2, 3, 4] # list head/tail patterns
inspect((a, b, tl)) # (1, 2, [3, 4])

let %{one, "two" => two} = %{one: 1, "two" => 2, unused: 0} # map patterns
inspect((one, two)) # (1, 2)

let _ = :something # wildcard pattern
# no bindings are created

let [x | xs] as ls = [1, 2, 3] # as patterns
inspect((x, xs, ls)) # (1, [2, 3], [1, 2, 3])
```

While pattern matching is most frequently used in let bindings, it is also used in `if let` expressions, `match` expressions,
and function arguments.

`if let` expressions are used for refutable patterns:

```ex
let result = (:ok, 5)
if let (:ok, n) = result then
    inspect(n)
else
    println("Error")
```

`match` expressions:

```ex
let ls = [1, 2, 3, 4]
match ls
    | [1 | xs] -> println("Starts with 1")
    | [_ | xs] -> println("Starts with something other than 1")
```

#### Closures:

```ex
let a = 5
let f = fn(x) => x * a # f captures a

inspect(f(2)) # 10

let g = fn(a, [x | xs]) = (a * x, xs) # pattern matching works in function arguments
inspect(g(1, [2, 3, 4])) # (2, [3, 4])
```

#### Named functions:

Named functions do not capture their environment. As a result, they run
slightly faster and can be made mutually recursive

```
let f(x) = x * 2
inspect(f(2)) # 4
```

#### Maps:

```ex
# pairs with non-atom keys use "=>" arrows
let x = %{"one" => 1, "two" => 2, "three" => 3}

# pairs with atom keys use colons
let y = %{one: 1, two: 2, three: 3}

# the following are equivalent:
%{one: 1, two: 2}
%{:one => 1, :two, 2}

# Maps are accessed via function call syntax
inspect(x("one")) # 1
inspect(y(:one)) # 1

# However it's often more convenient to pattern match over them,
# especially with atoms as keys

let %{"one" => one, "two" => two} = x
inspect((one, two)) # the three does not get bound

let %{one, two} = y # key punning, equivalent to the next line
let %{:one => one, :two => two} = y

# Maps can be updated using update syntax
let m = %{one: 1, two: 2}
let g = %{three: 3 | m}
inspect(g) # %{:one: 1, :three: 3, :two: 2}
```

#### Lists

```ex
# Lists are heterogenous linkedlists.
let ls = [1, 2, 5, 7]

# Generally, lists are accessed via pattern matching
let [a, b | tl] = ls
inspect((a, b, tl)) # (1, 2, [5, 7])

# They can also be accessed by index in O(n) time via the nth function
inspect(nth(ls, 2)) # 5

# Range expressions
inspect([1..10]) # [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
inspect([1,5..25]) # [1, 5, 9, 13, 17, 21]

# List comprehensions
inspect([n * n for n in [1..100] if n mod 12 == 0]) # [144, 576, 1296, 2304, 3600, 5184, 7056, 9216]
```

#### Captures and Pipes

```ex
# Currying is emulated via function captures

let polynomial = fn(a, b, c, x) => a * x * x + b * x + c
let f = polynomial(2, 3, 4, _)
let g = polynomial(_, _, _, 10)

inspect(f(10)) # 234
inspect(g(2, 3, 4)) # 234

# Captures are especially useful in combination with the pipe operator.
# The following code takes advantage of the standard add, sub, mul, and div functions
# as well as the fact that inspect returns its arguments unchanged after printing them

let f = polynomial(2, 3, 4)

10
|> f
|> inspect # 234
|> add(_, 10)
|> inspect # 244
|> div(_, 100)
|> inspect # 2.44
|> sub(1000, _)
|> inspect # 997.56
|> mul(_, 10)
|> inspect # -9975.599
```

### Build

```bash
dune build
```

Run a file using:

```bash
dune exec ./bin/rustscript_cli.exe <file>
```

Start a REPL using:

```bash
dune exec ./bin/rustscript_cli.exe
```

# Further examples

#### FizzBuzz
```ex
# ideally, for ... in will become a macro over foreach
let fizzbuzz(n) = foreach([1..101], fn(n) => match (n % 3, n % 5)
    | (0, 0) -> println("FizzBuzz")
    | (0, _) -> println("Fizz")
    | (_, 0) -> println("Buzz")
    | _ -> println(to_string(n))
)

fizzbuzz(100)
```

#### Quicksort

```ex
let sort = fn(ls) => match ls
    | [] -> []
    | [pivot | tail] -> {
	let (higher, lower) = partition(tail, fn(x) => x >= pivot)
        sort(lower) + [pivot] + sort(higher)
    }

inspect(sort([5, 3, 7, 9, 10, 4, 6])) # [3, 4, 5, 6, 7, 9, 10]
```

#### Run Length Encode
```ex
let run_len_encode = fn(ls) => match ls
    | [] -> []
    | [x | xs] -> {
        let next = run_len_encode(xs)
        match next
            | [(next_x, cnt) | tl] when x == next_x -> [(x, cnt + 1) | tl]
            | _ -> [(x, 1) | next]
    }

let test_ls = [1, 1, 2, 3, 4, 4, 4, 5, 6, 1, 2, 2]

# [(1., 2.), (2., 1.), (3., 1.), (4., 3.), (5., 1.), (6., 1.), (1., 1.), (2., 2.)]
inspect(run_len_encode(test_ls))
```

#### Binary Search Tree
```ex
let insert = fn(root, key) => match root
    | () -> %{val: key}
    | %{right, val} when val < key -> %{right: insert(right, key) | root}
    | %{left} -> %{left: insert(left, key) | root}

let tree_to_ls_inorder = {
    let loop = fn(root, acc) => match root
	| () -> acc
	| %{val, left, right} -> {
	    let acc = loop(left, acc)
	    let acc = [val | acc]
	    loop(right, acc)
	}

    fn(bst) => reverse(loop(bst, []))
}

let construct_from_list = fn(ls) =>
    fold((), fn(t, v) => insert(t, v), ls)

let ls = [50, 30, 20, 65, 42, 20, 40, 70, 60, 80]
let bst = construct_from_list(ls)
inspect(tree_to_ls_inorder(bst)) # [20, 20, 30, 40, 42, 50, 60, 65, 70, 80]
```

#### Two Sum
```ex
let two_sum = fn(nums, target) => {
    let helper = fn(m, ls, target) => match ls
        | [] -> ()
        | [(i, x) | xs] -> {
            let complement = target - x
            match m
                | %{complement => ()} -> helper(%{x: i | m}, xs, target)
                | %{complement => y} -> (y, i)
        }

    helper(%{}, enumerate(nums), target)
}

inspect(two_sum([1,9,13,20,47], 10)) # (0, 1)
inspect(two_sum([3,2,4,1,9], 10)) # (0, 4)
inspect(two_sum([], 10)) # ()
```

##### Caesar Cipher
```ex
let (to_number, to_letter) = {
    let enumerated = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> to_charlist |> enumerate
    let to_number = fold(%{}, fn(m, (i, l)) => %{l => i | m}, enumerated)
    let to_letter = fold(%{}, fn(m, (i, l)) => %{i => l | m}, enumerated)
    (to_number, to_letter)
}

let encode = fn(text, n) => {
    let shift = shift % 26

    let loop = fn(char_ls, acc) => match char_ls
	| [] -> concat(reverse(acc))
	| [c | xs] when to_number(c) == () -> loop(xs, [c | acc])
	| [c | xs] -> {
	    let new_letter = c
		|> to_number
		|> add(shift, _)
		|> fn(c) => if c < 0 then 26 + c else c
		|> fn(c) => to_letter(c % 26)
	    loop(xs, [new_letter | acc])
	}

    loop(to_charlist(text), [])
}

let decode = fn(text, n) => encode(text, -n)

inspect(encode("HELLO WORLD", 5)) # "MJQQT BTWQI"
inspect(decode(encode("HELLO WORLD", 5), 5)) # "HELLO WORLD"
```

##### Project Euler #1
```ex
euler1 = sum([x for x in [1..1000] if x % 3 == 0 || x % 5 == 0])
inspect(euler1) # 233168
```

##### Project Euler #2
```ex
let euler2 = {
    let aux = fn((a, b), acc) =>
        if b < 4000000 then 
            aux((b, a + 4 * b), acc + b)
       else 
            acc

    aux((0, 2), 0)
}

inspect(euler2) # 4613732
```

#### Euler 3
```ex
let gcd = fn(a, b) => match (a, b)
    | (0, x)|(x, 0) -> x
    | (a, b) when a > b -> gcd(b, a)
    | (a, b) -> {
        let remainder = b % a
        if remainder != 0 then (gcd(a, remainder)) else a
    }

let abs = fn(x) => if x < 0 then -x else x

let pollard = fn(n) => match n
    | 1 -> ()
    | n when n % 2 == 0 -> 2
    | n -> {
        let g = fn(x, n) => (x * x + 1) % n
        let iter = fn(x, y, d) => match (x, y, d)
            | (x, y, 1) -> {
                let x = g(x, n)
                let y = g(g(y, n), n)
                let d = gcd(abs(x - y), n)
                iter(x, y, d)
            }
            | (_, _, d) -> if d == n then () else d

        iter(2, 2, 1)
    }

let factor = fn(n) => {
    let d = pollard(n)
    if d == () then () else n / d
}

let euler3 = {
    # repeatedly factors until largest is found
    let aux = fn(n) => match factor(n)
        | () -> n
        | f when n == f -> f
        | f -> aux(f)

    let n = 600851475143
    aux(n)
}

inspect(euler3) # 6857
```

More project euler problems can be found in the [examples folder](https://github.com/mkhan45/RustScript2/tree/main/examples).
