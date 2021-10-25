let two_sum = fn(nums, target) => {
    let helper = fn(m, ls, target) => match ls
        | [] -> ()
        | [(i, x) | xs] -> {
            let complement = target - x
            match m
                | %{complement: ()} -> helper(%{x: i | m}, xs, target)
                | %{complement: y} -> (y, i)
        }

    helper(%{}, enumerate(nums), target)
}

# inspect(two_sum([1,9,13,20,47], 10)) # (0, 1)
# inspect(two_sum([3,2,4,1,9], 10)) # (0, 4)
# inspect(two_sum([], 10)) # ()
