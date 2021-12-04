let quicksort(ls) = match ls
    | [] -> []
    | [pivot | tail] -> {
	let (higher, lower) = partition(tail, fn(x) => x >= pivot)
        quicksort(lower) + [pivot] + quicksort(higher)
    }

# inspect(sort([5, 3, 7, 9, 10, 4, 6])) # [3, 4, 5, 6, 7, 9, 10]
