let (to_number, to_letter) = {
    let enumerated = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> to_charlist |> enumerate
    let to_number = fold(%{}, fn(m, (i, l)) => %{l => i | m}, enumerated)
    let to_letter = fold(%{}, fn(m, (i, l)) => %{i => l | m}, enumerated)
    (to_number, to_letter)
}

let encode = fn(text, n) => {
    let loop = fn(char_ls, n, acc) => match char_ls
	| [] -> concat(reverse(acc))
	| [c | xs] when to_number(c) == () -> loop(xs, n, [c | acc])
	| [c | xs] -> {
	    let new_letter = c
		|> to_number
		|> add(n, _)
		|> fn(n) => if n < 0 then 26 + n else n
		|> fn(n) => to_letter(n % 26)
	    loop(xs, n, [new_letter | acc])
	}

    loop(to_charlist(text), n % 26, [])
}

let decode = fn(text, n) => encode(text, -n)
