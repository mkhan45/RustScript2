let repeat(x, n) = {
    let helper(x, n, acc) = match n
	| 0 -> acc
	| n -> helper(x, n - 1, [x | acc])

    helper(x, n, [])
}

let empty_board() = repeat(:empty, 9)

let square_to_string(square) = match square
    | :empty -> " "
    | :x     -> "X"
    | :o     -> "O"

let nth(ls, i) = match i
    | 0 -> ^ls
    | i -> nth($ls, i - 1)

let set_nth(ls, i, x) = match i
    | 0 -> [x | $ls]
    | i -> [^ls | set_nth($ls, i - 1, x)]

let print_board(board) = {
    foreach([0..3], fn(i) => {
	let [a, b, c] = slice(board, 3 * i, 3 * i + 3)
	let (a, b, c) = (square_to_string(a), square_to_string(b), square_to_string(c))
	println(" " + a + " | " + b + " | " + c)
	if i != 2 then println("-----------") else ()
    })
}

let switch_turn(turn) = match turn
    | :x -> :o
    | :o -> :x

let is_winner(board, turn) = {
    let flatten(ls) = fold([], fn(a, b) => a + b, ls)

    let rows = ([slice(board, 3 * i, 3 * i + 3) for i in [0..3]])
    let cols = ([[nth(board, 3 * i + j) for i in [0..3]] for j in [0..3]])
    let diags = [[nth(board, i) for i in [0, 4, 8]], [nth(board, i) for i in [2, 4, 6]]]

    let sets = flatten([rows, cols, diags])
    any([
	all([
	    sq == turn
	    for sq in set
	])
	for set in sets
    ])
}

let loop(board, turn) = {
    println("===========\n")
    print_board(board)
    print("\nChoose a position for " + square_to_string(turn) + ": ")

    let get_position () = match string_to_num(scanln())
	| (:ok, n) when (0 <= n) && (n <= 8) -> n
	| _ -> {
	    println("\nError: enter a number from 0 to 9")
	    get_position()
	}

    let position = get_position()
    let new_board = set_nth(board, position, turn)

    if is_winner(new_board, turn) then {
	println(square_to_string(turn) + " Wins!")
	print_board(board)
    } else {
	loop(set_nth(board, position, turn), switch_turn(turn))
    }
}

loop(empty_board(), :x)
