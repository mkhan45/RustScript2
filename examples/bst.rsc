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

let tree_to_ls_preorder = {
    let loop = fn(root, acc) => match root
	| () -> acc
	| %{val, left, right} -> {
	    let acc = [val | acc]
	    let acc = loop(left, acc)
	    loop(right, acc)
	}

    fn(bst) => reverse(loop(bst, []))
}

let construct_from_list = fn(ls) =>
    fold((), fn(t, v) => insert(t, v), ls)

let ls = to_charlist("khan348kha")
let bst = construct_from_list(ls)

let str_cmp(a, b) = match T
    | _ when a < b -> -1
    | _ when a == b -> 0
    | _ when a > b -> 1
