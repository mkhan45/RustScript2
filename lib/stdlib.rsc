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
    | [x | xs] -> if f(x) then x else f(xs)

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

let zip(ls) = reverse(zip_rev(ls))

let length(ls) = fold(0, fn(l, _) => l + 1, ls)

let enumerate_rev(ls) = {
    let len = length(ls)
    zip_rev([0..len], ls)
}

let enumerate(ls) = reverse(enumerate_rev(ls))

let concat(ls) = fold("", fn(a, b) => a + b, ls)

let concat_sep = fn(ls, sep) => fold("", fn(a, b) => a + b + sep, ls)

let sum(ls) = fold(0, fn(a, b) => a + b, ls)
let any(ls) = fold(F, fn(a, b) => a || b, ls)
let all(ls) = fold(T, fn(a, b) => a && b, ls)

let foreach(ls, f) = match ls
    | [] -> ()
    | [x | xs] -> {
	f(x)
	foreach(xs, f)
    }

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

let max(a, b) = if a > b then a else b
let min(a, b) = if a < b then a else b
