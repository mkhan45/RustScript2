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

const rustscript_tostring = v => {
    if (typeof v === "string") {
        return v
    } else if (Array.isArray(v)) {
        return `(${v.map(rustscript_tostring).join()})`
    } else if (v === undefined) {
        return "undefined";
    } else if (v !== null && v.val) {
        return ll_to_string(v);
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

const rsc_inspect = val => console.log(rustscript_tostring(val));
const rsc_print = val => process.stdout.write(val);
const rsc_println = val => console.log(val);

const range_step = (start, end, step) => {
    if (start >= end) {
        return null;
    } else {
        return {val: start, next: range_step(start + step, end, step)};
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
