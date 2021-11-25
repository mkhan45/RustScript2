let merge_maps(m1, m2) = {
    let m1_list = map_to_list(m1)
    fold(m2, fn(acc, (k, v)) => %{k => v | acc}, m1_list)
}

let count(ls) = {
    let loop(ls, counter) = match ls
	| [] -> counter
	| [x | xs] when counter(x) == () -> loop(xs, %{x => 1 | counter})
	| [x | xs] -> loop(xs, %{x => counter(x) + 1 | counter})

    loop(ls, %{})
}

let reverse(ls) = {
    let fold_step = fn(ls, x) => [x|ls]
    fold([], fold_step, ls)
}

let filter_rev(f, ls) = {
    let fold_step = fn(ls, x) => if f(x) then [x|ls] else ls
    fold([], fold_step, ls)
}

let filter(f, ls) = reverse(filter_rev(f, ls))

let find(f, ls) = match ls
    | [] -> ()
    | [x | xs] -> if f(x) then x else find(f, xs)

let map_rev(f, ls) = fold([], fn(ls, x) => [f(x)|ls], ls)
let map(f, ls) = reverse(map_rev(f, ls))
let range(a, b) = range_step(a, b, 1)

let zip_rev(l1, l2) = {
    let helper = fn(acc, l1, l2) => match (l1, l2)
	| ([], _) -> acc
	| (_, []) -> acc
	| ([x|xs], [y|ys]) -> helper([(x, y)|acc], xs, ys)

    helper([], l1, l2)
}

let zip(l1, l2) = reverse(zip_rev(l1, l2))

let length(ls) = fold(0, fn(l, _) => l + 1, ls)

let enumerate_rev(ls) = {
    let len = length(ls)
    zip_rev([0..len], ls)
}

let enumerate(ls) = reverse(enumerate_rev(ls))

let concat(ls) = fold("", fn(a, b) => a + b, ls)

let intersperse_rev(ls, sep) = {
    let loop(ls, sep, acc) = match ls
	| [] -> acc
	| [x] -> [x | acc]
	| [x | xs] -> loop(xs, sep, [sep, x | acc])

    loop(ls, sep, [])
}

let intersperse(ls, sep) = reverse(intersperse_rev(ls, sep))

let concat_sep(ls, sep) = concat(intersperse(ls, sep))

let is_sorted_by(ls, f) = match ls
    | [] | [_] -> T
    | [a | [b | _] as xs] when f(a, b) == :less || f(a, b) == :equal -> is_sorted_by(xs, f)
    | _ -> F

let op_cmp(a, b) = match T
    | _ when a < b  -> :less
    | _ when a == b -> :equal
    | _             -> :more

let is_sorted(ls) = is_sorted_by(ls, op_cmp)

let sum(ls) = fold(0, fn(a, b) => a + b, ls)
let any(ls) = fold(F, fn(a, b) => a || b, ls)
let all(ls) = fold(T, fn(a, b) => a && b, ls)

let foreach(ls, f) = match ls
    | [] -> ()
    | [x | xs] -> {
	f(x)
	foreach(xs, f)
    }

let group_by(ls, f) = {
    let fold_step = fn(acc, el) => match acc
	| [] when f(el) -> [[]]
	| [] -> [[el]]
	| [current | _] when f(el) -> [[] | acc]
	| [current | rest] -> [[el | current] | rest]

    fold([], fold_step, ls) |> reverse |> map(reverse, _)
}

let split(ls, el) = group_by(ls, eq(_, el))

# copied from https://github.com/janestreet/base/blob/0f626a86991b020348eac9aa0244d59da43ae02c/src/list.ml#L1060
let split_at(n, ls) = 
    if n <= 0 then {
	([], ls)
    } else {
	let loop(n, xs, acc) =
	    if n == 0 then 
		(reverse(acc), xs)
	    else match xs
		| [] -> (ls, acc)
		| [x | xs] -> loop(n - 1, xs, [x | acc])
	
	loop(n, ls, [])
    }

let take(n, ls) = {
    let (res, _) = split_at(n, ls)
    res
}

let drop(n, ls) =
    if n <= 0 then
	ls
    else
	drop(n - 1, $ls)

let slice(ls, start, end) = take(end - start, drop(start, ls))

let take_while(ls, f) = {
    let loop(ls, f, acc) = match ls
	| [x | xs] when f(x) -> loop(xs, f, [x | acc])
	| _ -> reverse(acc)

    loop(ls, f, [])
}

let skip_while(ls, f) = match ls
    | [x | xs] when f(x) -> skip_while(xs, f)
    | _ -> ls

let max(a, b) = if a > b then a else b
let min(a, b) = if a < b then a else b

let partition_rev(ls, f) = {
    let loop(ls, f, (l1, l2)) = match ls
	| [] -> (l1, l2)
	| [x | xs] when f(x) -> loop(xs, f, ([x | l1], l2))
	| [x | xs] -> loop(xs, f, (l1, [x | l2]))

    loop(ls, f, ([], []))
}

let partition(ls, f) = {
    let (l1, l2) = partition_rev(ls, f)
    (reverse(l1), reverse(l2))
}

let step_by(ls, n) = {
    let loop(ls, n, i, acc) = match (ls, i)
	| ([], _) -> reverse(acc)
	| ([x | xs], 0) -> loop(xs, n, n, [x | acc])
	| ([_ | xs], i) -> loop(xs, n, i - 1, acc)

    loop(ls, n - 1, 0, [])
}

let repeat(x, n) = {
    let helper(x, n, acc) = match n
	| 0 -> acc
	| n -> helper(x, n - 1, [x | acc])

    helper(x, n, [])
}

let nth(ls, i) = match i
    | 0 -> ^ls
    | i -> nth($ls, i - 1)

let set_nth(ls, i, x) = match i
    | 0 -> [x | $ls]
    | i -> [^ls | set_nth($ls, i - 1, x)]

let merge = fn(xs, ys, cmp) => match (xs, ys)
    | (ls, [])|([], ls) -> ls
    | ([x|xs], [y|ys]) when cmp(x, y) <= 0 -> [x | merge(xs, [y|ys], cmp)]
    | ([x|xs], [y|ys]) -> [y | merge([x|xs], ys, cmp)]

let sort = fn(ls, cmp) => {
    let pairs = fn(ls) => match ls
        | [a, b | tl] -> [merge(a, b, cmp) | pairs(tl)]
        | _ -> ls

    let loop = fn(ls) => match ls
        | [x] -> x
        | _ -> loop(pairs(ls))

    loop([[x] for x in ls])
}

let abs(x) = if x >= 0 then x else -x

let replace(ls, pairs) = {
    let loop = fn(ls, acc) => match ls
	| [] -> reverse(acc)
	| [x | xs] when pairs(x) != () -> loop(xs, [pairs(x) | acc])
	| [x | xs] -> loop(xs, [x | acc])

    loop(ls, [])
}

let add(a, b) = a + b
let sub(a, b) = a - b
let mul(a, b) = a * b
let div(a, b) = a / b
let eq(a, b) = a == b
let geq(a, b) = a >= b
let leq(a, b) = a <= b
let lt(a, b) = a < b
let gt(a, b) = a > b

let inspect(n) = inspect__builtin(n)
let print(n) = print__builtin(n)
let println(n) = println__builtin(n)
let scanln() = scanln__builtin()
let to_string(n) = to_string__builtin(n)
let string_to_num(n) = string_to_num__builtin(n)
let string_to_int(n) = string_to_int__builtin(n)
let range_step(start, end, step) = range_step__builtin(start, end, step)
let fold(acc, f, ls) = fold__builtin(acc, f, ls)
let to_charlist(n) = to_charlist__builtin(n)
let get(m, k) = get__builtin(m, k)
let read_file(file) = read_file__builtin(file)
let write_file(file, data) = write_file__builtin(file, data)
let list_dir(dir) = list_dir__builtin(dir)
let map_keys(m) = map_keys__builtin(m)
let map_to_list(m) = map_to_list__builtin(m)
let typeof(m) = typeof__builtin(m)
let start_server(port, callback) = serve__builtin(port, callback)
let start_server_ssl(cert_path, key_path, port, callback) = serve_ssl__builtin(cert_path, key_path, port, callback)
