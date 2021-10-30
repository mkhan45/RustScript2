let (to_number, to_letter) = {
    let letter_arr = to_charlist("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    let enumerated = enumerate(letter_arr)
    let to_number = fold(%{}, fn(m, (i, l)) => %{l => i | m}, enumerated)
    let to_letter = fold(%{}, fn(m, (i, l)) => %{i => l | m}, enumerated)
    (to_number, to_letter)
}

let encode = fn(text, n) => {
    let loop = fn(char_ls, n, acc) => match char_ls
	| [] -> concat(reverse(acc))
	| [c | xs] when to_number(c) == () -> loop(xs, n, [c | acc])
	| [c | xs] -> {
	    let num = to_number(c) + n
	    let num = if num < 0 then 26 + num else num
	    let new_letter = to_letter(num % 26)
	    loop(xs, n, [new_letter | acc])
	}

    loop(to_charlist(text), n % 26, [])
}

let decode = fn(text, n) => encode(text, -n)
