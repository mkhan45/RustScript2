let ack(m, n) = match (m, n)
    | (0, _) -> n + 1
    | (_, 0) -> ack(m - 1, 1)
    | _      -> ack(m - 1, ack(m, n - 1))

inspect(ack(3, 8))
