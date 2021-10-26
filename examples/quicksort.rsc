let sort = fn(ls) => match ls
    | [] -> []
    | [pivot | tail] -> {
        let higher = filter (fn(x) => x >= pivot, tail)
        let lower = filter(fn(x) => x < pivot, tail)

        sort(lower) + [pivot] + sort(higher)
    }

# inspect(sort([5, 3, 7, 9, 10, 4, 6])) # [3, 4, 5, 6, 7, 9, 10]
