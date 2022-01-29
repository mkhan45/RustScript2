
const ll_from_ls = ls => {
    let res = null;
    for (let i = ls.length - 1; i >= 0; i += 1)
        res = {val: ls[i], next: res}
    
    return res;
}

const rustscript_tostring = v => {
    if (typeof v === "string") {
        return v
    } else if (Array.isArray(v)) {
        return `[${v.map(rustscript_tostring).join()}]`
    } else if (v === undefined) {
        return "undefined";
    } else {
        return v.toString();
    }
}

const rustscript_equal = (a, b) => {
    if (a === b) {
        return true;
    } else if (Array.isArray(a) && Array.isArray(b)) {
        return (a.length === b.length) && (a.every((v, i) => v == b[i]));
    }
}

var __ident_f = (__ident_x, __ident_y) => (_ => {
var __ident_c = __ident_x * __ident_y;

return __ident_c + __ident_x + __ident_y
})();
var __ident_t = [__ident_f(10, 5), __ident_f(5, 10)];
let [__ident_a, __ident_b] = __ident_t;
var __ident_f = ([__ident_a, __ident_b], __ident_c) => __ident_a * __ident_b + __ident_c;
console.log(rustscript_tostring(__ident_f([5, 10], 15)));
const __ident_g = (__ident_x) => __ident_x * 2;
console.log(rustscript_tostring(__ident_g(5)));
const __ident_fib = (__ident_x) => (_ => {if (__ident_x < 2) { return (_ => __ident_x)() } else { return (_ => __ident_fib(__ident_x - 1) + __ident_fib(__ident_x - 2))() }})();
console.log(rustscript_tostring(__ident_fib(10)));
const __ident_range = (__ident_l, __ident_r) => (_ => {if (rustscript_equal(__ident_l, __ident_r)) { return (_ => [])() } else { return (_ => [__ident_l, __ident_range(__ident_l + 1, __ident_r)])() }})();
console.log(rustscript_tostring(__ident_range(5, 15)));
const __ident_map = (__ident_f, __ident_ls) => (_ => {if (rustscript_equal(__ident_ls, [])) { return (_ => [])() } else { return (_ => (_ => {
console.log(rustscript_tostring(__ident_ls));
let [__ident_hd, __ident_tl] = __ident_ls;

return [__ident_hd, __ident_map(__ident_f, __ident_tl)]
})())() }})();
console.log(rustscript_tostring(__ident_map(__ident_fib, __ident_range(1, 10))));
