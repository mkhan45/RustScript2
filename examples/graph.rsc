let y_bin() = 1

let distance_sqr((x1, y1), (x2, y2)) = {
    let x = x1 - x2
    let y = y1 - y2
    x * x + y * y
}

let draw_graph(points, scale) = {
    let height = 20
    let width = 40

    let col_loop = fn(row, col, row_arr) => {
	if col == width then {
	    ["\n" | row_arr]
	} else {
	    let current_point = ((col - width / 2) / scale, (row - height / 2) / scale)
	    let is_filled = points
		|> map(distance_sqr(current_point, _), _)
		|> map(fn(dist) => dist < 0.001, _)
		|> any

	    let point_char = if is_filled then "." else " "
	    col_loop(row, col + 1, [point_char | row_arr])
	}
    }

    let row_loop = fn(row, table) => {
	if row == height then {
	    table
	    |> map(reverse, _)
	    |> map(concat, _)
	    |> concat
	} else {
	    let table = [col_loop(row, 0, []) | table]
	    row_loop(row + 1, table)
	}
    }

    let graph_str = row_loop(0, [])
    println(graph_str)
}

let f(x) = x * x * x

let points = [-15..16]
    |> map(div(_, 20), _)
    |> map(fn(x) => (x, f(x)), _)

draw_graph(points, 20)
