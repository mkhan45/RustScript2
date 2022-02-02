const _ = this._;
const assert = console.assert;

const ll_bind_n = (ls, n) => {
    let result = [];
    let current = ls;
    for (let i = 0; i < n; i += 1) {
        result.push(current.val);
        current = current.next;
    }

    assert(current === null);
    return result;
}

const ll_bind_head_tail = (ls, n) => {
    let result = [];
    let current = ls;
    for (let i = 0; i < n; i += 1) {
        result.push(current.val);
        current = current.next;
    }

    result.push(current);
    return result;
}

const ll_to_string = ls => {
    let result = [];

    while (ls !== undefined && ls !== null) {
        result.push(ls.val);
        ls = ls.next;
    }

    return `[${result}]`;
}

const ll_from_ls = ls => {
    let res = null;
    for (let i = ls.length - 1; i >= 0; i -= 1)
        res = {val: ls[i], next: res}
    
    return res;
}

const prepend_arr = (arr, ll) => {
    assert(ll === null || ll.val !== null);

    let new_ll = ll;
    for (let i = arr.length - 1; i >= 0; i -= 1)
        new_ll = {val: arr[i], next: new_ll};

    return new_ll;
}

const to_string__builtin = v => {
    if (typeof v === "string") {
        return `"${v}"`
    } else if (Array.isArray(v)) {
        return `(${v.map(to_string__builtin).join()})`
    } else if (v.__is_rsc_map) {
        let res = [];
        for (const [key, val] of v.map.entries()) {
            res.push(`${to_string__builtin(key)} => ${to_string__builtin(val)}`);
        }
        return `%{\n\t${res.join("\n\t")}\n}`
    }else if (v === undefined) {
        return "undefined";
    } else if (v !== null && v.val) {
        return ll_to_string(v);
    } else {
        return JSON.stringify(v);
    }
}

const rustscript_equal = (a, b) => {
    if (a === b) {
        return true;
    } else if (Array.isArray(a) && Array.isArray(b)) {
        return (a.length === b.length) && (a.every((v, i) => v == b[i]));
    }
}

const rsc_matches = (val, pat) => {
    if (pat === null || val === pat) {
        return true;
    }

    if (Array.isArray(val) && Array.isArray(pat)) {
        return _(val).zip(pat).every(([v, p]) => rsc_matches(v, p));
    }

    // or pattern
    if (pat.__rsc_pat_type === 0) {
        return rsc_matches(val, pat.l) || rsc_matches(val, pat.r);
    }

    return false;
}

const inspect__builtin = val => console.log(to_string__builtin(val));
const print__builtin = val => process.stdout.write(val);
const println__builtin = val => console.log(val);
const string_to_num__builtin = parseFloat;
const string_to_int__builtin = parseInt

const range_step__builtin = (start, end, step) => {
    if (start >= end) {
        return null;
    } else {
        return {val: start, next: range_step__builtin(start + step, end, step)};
    }
}

const mk_thunk = (fn, args) => {
    return {
        __rsc_is_thunk: true,
        fn: fn,
        args: args,
    }
}

const unwrap_thunk = thunk => {
    let res = thunk;
    while (res !== undefined && res.__rsc_is_thunk) {
        res = res.fn(...res.args)
    }

    return res;
}

const __rsc_mk_map = m => {
    let f = x => m.get(x);
    f.map = m;
    f.__is_rsc_map = true;
    return f;
}
