let insert = fn(root, key) => match root
    | () -> %{:val: key}
    | %{:val: root_val, :right: right} when root_val < key ->
	%{:right: insert(right, key) | root}
    | %{:val: root_val, :left: left} ->
	%{:left: insert(left, key) | root}

let tree_to_ls_inorder = {
    let loop = fn(root, acc) => match root
	| () -> acc
	| %{:val: v, :left: l, :right: r} -> {
	    let acc = loop(l, acc)
	    let acc = [v | acc]
	    let acc = loop(r, acc)
	    acc
	}

    fn(bst) => reverse(loop(bst, []))
}

let construct_from_list = fn(ls) =>
    fold((), fn(t, v) => insert(t, v), ls)

let ls = [50, 30, 20, 65, 42, 20, 40, 70, 60, 80]
let bst = construct_from_list(ls)
