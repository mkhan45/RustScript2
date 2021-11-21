let (to_number, to_letter) = {
    let enumerated = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> to_charlist |> enumerate
    let to_number = fold(%{}, fn(m, (i, l)) => %{l => i | m}, enumerated)
    let to_letter = fold(%{}, fn(m, (i, l)) => %{i => l | m}, enumerated)
    (to_number, to_letter)
}

let encode = fn(text, shift) => {
    let shift = shift mod 26

    let loop = fn(char_ls, acc) => match char_ls
	| [] -> concat(reverse(acc))
	| [c | xs] when to_number(c) == () -> loop(xs, [c | acc])
	| [c | xs] -> {
	    let new_letter = c
		|> to_number
		|> add(shift, _)
		|> fn(c) => if c < 0 then 26 + c else c
		|> fn(c) => to_letter(c mod 26)
	    loop(xs, [new_letter | acc])
	}

    loop(to_charlist(text), [])
}

let decode = fn(text, n) => encode(text, -n)
