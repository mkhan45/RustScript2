let euler2 = {
    let aux = fn((a, b), acc) =>
        if b < 4000000 then 
	    aux((b, a + 4 * b), acc + b)
       else 
	   acc

    aux((0, 2), 0)
}
