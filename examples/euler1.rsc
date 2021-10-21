let range = {
    let helper = fn (l, r, acc) => 
        if l == r then acc else helper(l, r - 1, (r, acc))
    
    fn (l, r) => helper(l - 1, r, ())
}

let filter = {
    let helper = fn(f, (hd, tl), acc) => {
        if tl == () then 
            acc 
        else if f(hd) then 
            helper(f, tl, (hd, acc))
        else 
            helper(f, tl, acc)
    }

    fn(f, ls) => helper(f, ls, ())
}

let sum = {
    let helper = fn((hd, tl), acc) =>
        if tl == () then (hd + acc) else helper(tl, hd + acc)
    
    fn (ls) => helper(ls, 0)
}

let predicate = fn(n) => (n % 3 == 0) || (n % 5 == 0)
