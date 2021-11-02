let fizzbuzz(n) = foreach([1..101], fn(n) => match (n % 3, n % 5)
    | (0, 0) -> println("FizzBuzz")
    | (0, _) -> println("Fizz")
    | (_, 0) -> println("Buzz")
    | _ -> println(to_string(n))
)

fizzbuzz(100)
