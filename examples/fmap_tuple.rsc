let fmap = fn (f, ls) => {
    if ls == () then {
        ()
    } else {
        let (hd, tl) = ls
        (f(hd), fmap(f, tl))
    }
}

let f = fn(x) => x * 2

fmap(f, (5, (10, (20, (30, (1, ()))))))
