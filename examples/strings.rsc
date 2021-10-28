let a = "abc"

let b = "123!"

let result = match (a, b, a + b)
    | ("abc", "123!", "abc123!") -> T
    | _ -> F
