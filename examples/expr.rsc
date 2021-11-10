let contains(ls, el) = match ls
    | [] -> F
    | [x | xs] when x == el -> T
    | [_ | xs] -> contains(xs, el)

let digits() = to_charlist("0123456789")

let is_digit(c) = contains(digits(), c)

let chars_to_number(chars) = {
    let digit_map = {
	let enumerated = enumerate(digits())
	fold(%{}, fn(m, (i, d)) => %{d => i | m}, enumerated)
    }

    let chars = reverse(chars)
    let loop = fn(ls, acc, multiplier) => match ls
	| [] -> acc
	| [c | cs] -> {
	    let number = digit_map(c) * multiplier
	    loop(cs, acc + number, multiplier * 10)
	}

    loop(chars, 0, 1)
}

let scan(str) = {
    let char_ls = to_charlist(str)
    
    let loop_number = fn(ls, acc) => match ls
	| [c | xs] when is_digit(c) -> loop_number(xs, [c | acc])
	| _ -> (reverse(acc), ls)


    let loop = fn(ls, acc) => match ls
	| [] -> reverse(acc)
	| [" " | xs] -> loop(xs, acc)
	| ["+" | xs] -> loop(xs, [:add | acc])
	| ["-" | xs] -> loop(xs, [:sub | acc])
	| ["*" | xs] -> loop(xs, [:mul | acc])
	| ["/" | xs] -> loop(xs, [:div | acc])
	| [c | _]  -> {
	    let (digit_chars, xs) = loop_number(ls, [])
	    let number = chars_to_number(digit_chars)
	    loop(xs, [(:number, number) | acc])
	}

    loop(to_charlist(str), [])
}

let op_bp(op) = match op
    | :add | :sub -> (1, 2)
    | :mul | :div -> (3, 4)

let complete_expr(lhs, ls, min_bp) = match ls
    | [(:number, _) | _] | [] -> (lhs, ls)
    | [op | xs] -> {
	let (l_bp, r_bp) = op_bp(op)
	if l_bp < min_bp then {
	    (lhs, ls)
	} else {
	    let (rhs, rest) = expr_bp(xs, r_bp)
	    let complete = %{op: op, lhs: lhs, rhs: rhs}
	    complete_expr(complete, rest, min_bp)
	}
    }

let expr_bp(toks, min_bp) = match toks
	| [(:number, _) as n | xs] -> complete_expr(n, xs, min_bp)
	| _ -> let () = 1

let eval(expr) = match expr
    | %{op: :add, lhs: l, rhs: r} -> eval(l) + eval(r)
    | %{op: :sub, lhs: l, rhs: r} -> eval(l) - eval(r)
    | %{op: :mul, lhs: l, rhs: r} -> eval(l) * eval(r)
    | %{op: :div, lhs: l, rhs: r} -> eval(l) / eval(r)
    | (:number, n) -> n
    | _ -> expr

let eval_str(s) = {
    let tokens = scan(s)
    let (expr, _) = expr_bp(tokens, 0)
    eval(expr)
}

let input_loop() = {
    print("Enter an expression to evaluate: ")

    match scanln ()
	| () -> println("\nNo line scanned, exiting")
	| "exit" -> println("exiting")
	| line -> {
	    let res = eval_str(line)
	    println(to_string(res))
	    input_loop()
	}
}
#loop()
