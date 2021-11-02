let fizzbuzz(n) = foreach([1..101], fn(n) => match (n % 3, n % 5)
    | (0, 0) -> inspect("FizzBuzz")
    | (0, _) -> inspect("Fizz")
    | (_, 0) -> inspect("Buzz")
    | _ -> inspect(n)
)

fizzbuzz(100)
