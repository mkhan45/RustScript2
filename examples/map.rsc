let m = %{
    1 => 2, 
    3 => 4, 
    (5, 6) => (7, 8)
}

let %{1 => x, 3 => y, (5, 6) => z, 467 => a} = m
