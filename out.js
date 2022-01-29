
var __ident_f = (__ident_x, __ident_y) => {
var __ident_c = __ident_x * __ident_y;

return __ident_c + __ident_x + __ident_y
};
var __ident_t = [__ident_f(10, 5), __ident_f(5, 10)];
var [__ident_a, __ident_b] = __ident_t;
var __ident_f = ([__ident_a, __ident_b], __ident_c) => __ident_a * __ident_b + __ident_c;
console.log(__ident_f([5, 10], 15));
const __ident_g = (__ident_x) => __ident_x * 2;
console.log(__ident_g(5));
const __ident_fib = (__ident_x) => __ident_x < 2 ? __ident_x : __ident_fib(__ident_x - 1) + __ident_fib(__ident_x - 2);
console.log(__ident_fib(10));
