let x = :a1
let y = :b2

let m = %{:a: 1, :b: %{:a: 3, :b: 4, :c: 5}}
let %{:a: i, :b: m2} = m

let %{:a: z, :b: x, :c: y} = m2

inspect((i, z, x, y))
