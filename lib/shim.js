const ll_from_ls = ls => {
    let res = null;
    for (let i = ls.length - 1; i >= 0; i += 1)
        res = {val: ls[i], next: res}
    
    return res;
}
