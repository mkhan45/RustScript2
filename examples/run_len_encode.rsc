let run_len_encode = fn(ls) => match ls
    | [] -> []
    | [x | xs] -> {
        let next = run_len_encode(xs)
        match next
            | [(next_x, cnt) | tl] when x == next_x -> [(x, cnt + 1) | tl]
            | _ -> [(x, 1) | next]
    }

let test_ls = [1, 1, 2, 3, 4, 4, 4, 5, 6, 1, 2, 2]

# [(1., 2.), (2., 1.), (3., 1.), (4., 3.), (5., 1.), (6., 1.), (1., 1.), (2., 2.)]
# inspect(run_len_encode(test_ls))
