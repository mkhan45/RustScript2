open Types
open Stdio
open Base

let val_add lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs +. rhs)
    | _ -> 
            printf "Invalid Add: lhs = %s, rhs = %s\n" (string_of_val lhs) (string_of_val rhs);
            assert false

let val_sub lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs -. rhs)
    | _ -> assert false

let val_mul lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs *. rhs)
    | _ -> 
            printf "Invalid Mul: lhs = %s, rhs = %s\n" (string_of_val lhs) (string_of_val rhs);
            assert false

let val_div lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs /. rhs)
    | _ -> assert false

let val_is_true = function
    | Boolean true -> true
    | _ -> false

let rec val_eq lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Boolean (Float.equal lhs rhs)
    | Boolean lhs, Boolean rhs -> Boolean (Bool.equal lhs rhs)
    | Tuple lhs, Tuple rhs ->
            if phys_equal (List.length lhs) (List.length rhs)
                then
                    let zipped = List.zip_exn lhs rhs in
                    let res = List.for_all zipped ~f:(fun (a, b) -> val_is_true (val_eq a b))
                    in Boolean res
                else
                    Boolean false
    | _ -> Boolean false

let val_neq lhs rhs = Boolean (not (val_is_true (val_eq lhs rhs)))

let val_lt lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Boolean (Float.compare lhs rhs < 0)
    | _ -> assert false

let val_gt lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Boolean (Float.compare lhs rhs > 0)
    | _ -> assert false

let val_and lhs rhs = match lhs, rhs with
    | Boolean lhs, Boolean rhs -> Boolean (lhs && rhs)
    | _ -> assert false

let val_or lhs rhs = match lhs, rhs with
    | Boolean lhs, Boolean rhs -> Boolean (lhs || rhs)
    | _ -> assert false

let val_mod lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Number (Float.mod_float lhs rhs)
    | _ -> assert false
