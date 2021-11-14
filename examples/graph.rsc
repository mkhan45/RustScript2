let y_bin() = 0.5

let draw_graph(points) = {
    let height = 40.0
    let width = 80.0

    let loop(points, current_x) = match points
	| [] -> ()
	| [(x, y1) | [(_, y2) | _] as xs] when abs(y1 - y2) > y_bin() -> {
	    let spaces = concat(repeat(" ", x - current_x))
	    println(spaces + ".")
	    loop(xs, 0)
	}
	| [(x, _) | xs] -> {
	    let spaces = concat(repeat(" ", x - current_x))
	    print(spaces + ".")
	    loop(xs, x)
	}

    loop(points, 0)
    println("")
}

let f(x) = x * x

# sort in increasing y, then increasing x order
let sort_fn((x1, y1), (x2, y2)) = if abs(y1 - y2) < y_bin() then x1 - x2 else y2 - y1

let domain = map(div(_, 10), [-40..41])
let points = enumerate(map(f, domain))
let points = sort(points, sort_fn)

draw_graph(points)
