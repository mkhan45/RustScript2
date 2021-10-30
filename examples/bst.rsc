let sort = fn(ls) => match ls
    | [] -> []
    | [pivot | tail] -> {
        let higher = filter (fn(x) => x >= pivot, tail)
        let lower = filter(fn(x) => x < pivot, tail)

        sort(lower) + [pivot] + sort(higher)
    }

let insert = fn(root, key) => match root
    | () -> %{val: key}
    | %{right: right} when root(:val) < key ->
	%{right: insert(right, key) | root}
    | %{left: left} ->
	%{left: insert(left, key) | root}

let tree_to_ls_inorder = {
    let loop = fn(root, acc) => match root
	| () -> acc
	| %{val: v, left: l, right: r} -> {
	    let acc = loop(l, acc)
	    let acc = [v | acc]
	    loop(r, acc)
	}

    fn(bst) => reverse(loop(bst, []))
}

let construct_from_list = fn(ls) =>
    fold((), fn(t, v) => insert(t, v), ls)

let ls = [50, 30, 20, 65, 42, 20, 40, 70, 60, 80]
let bst = construct_from_list(ls)
