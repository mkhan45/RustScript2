let reverse(ls) = {
    let fold_step = fn(ls, x) => [x|ls]
    fold([], fold_step, ls)
}

let filter_rev(f, ls) = {
    let fold_step = fn(ls, x) => if f(x) then [x|ls] else ls
    fold([], fold_step, ls)
}

let filter(f, ls) = reverse(filter_rev(f, ls))

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
