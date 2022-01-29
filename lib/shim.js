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
