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
	all([sq == turn for sq in set])
	for set in sets
    ])
}

let max_value(board, turn) = {
    if is_winner(board, turn) then {
	(1, board)
    } else if is_winner(board, switch_turn(turn)) then {
	(-1, board)
    } else if !any([sq == :empty for sq in board]) then {
	(0, board)
    } else {
	let possible_positions = [i for (i, p) in enumerate_rev(board) if p == :empty]
	let init = (-99999, board)
	let fold_step = fn((max_val, max_board), position) => {
	    let current_board = set_nth(board, position, turn)
	    let (min_val, next_state) = min_value(current_board, switch_turn(turn))
	    if min_val > max_val then 
		(min_val, next_state)
	    else
		(max_val, max_board)
	}

	fold(init, fold_step, possible_positions)
    }
}

let min_value(board, turn) = {
    if is_winner(board, turn) then {
	(-1, board)
    } else if is_winner(board, switch_turn(turn)) then {
	(1, board)
    } else if !any([sq == :empty for sq in board]) then {
	(0, board)
    } else {
	let possible_positions = [i for (i, p) in enumerate(board) if p == :empty]
	let init = (99999, board)
	let fold_step = fn((min_val, min_board), position) => {
	    let current_board = set_nth(board, position, turn)
	    let (max_val, next_state) = max_value(current_board, switch_turn(turn))
	    if max_val > min_val then 
		(max_val, next_state)
	    else
		(min_val, min_board)
	}

	fold(init, fold_step, possible_positions)
    }
}

let minimax(board, turn, goal_turn) =
    if turn == goal_turn then
	max_value(board, turn)
    else
	min_value(board, turn)

let loop(board, turn) = {
    println("===========\n")
    print_board(board)
    print("\nChoose a position for " + square_to_string(turn) + ": ")

    let get_position = fn() => match string_to_num(scanln())
	| (:ok, n) when nth(board, n) != :empty -> {
	    print("That position is already taken, enter another: ")
	    get_position()
	}
	| (:ok, n) when (0 <= n) && (n <= 8) -> n
	| _ -> {
	    print("Error: position must be a number from 0 to 9: ")
	    get_position()
	}

    let position = get_position()
    let new_board = set_nth(board, position, turn)

    if is_winner(new_board, turn) then {
	println(square_to_string(turn) + " Wins!")
	print_board(new_board)
    } else {
	loop(set_nth(board, position, turn), switch_turn(turn))
    }
}

# loop(empty_board(), :x)
let board = empty_board()
let board = set_nth(board, 0, :x)
let board = set_nth(board, 1, :y)
let board = set_nth(board, 2, :x)
inspect(minimax(board, :x, :x))
