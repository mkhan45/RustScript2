open Types
open Stdio
open Base

let val_add lhs rhs ss = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs +. rhs)
    | ValList lhs, ValList rhs ->  ValList (lhs @ rhs)
    | StringVal lhs, StringVal rhs ->  StringVal (lhs ^ rhs)
    | _ -> 
            printf "Invalid Add: lhs = %s, rhs = %s\n" (string_of_val ss lhs) (string_of_val ss rhs);
            assert false

let val_sub lhs rhs _ss = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs -. rhs)
    | _ -> assert false

let val_mul lhs rhs ss = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs *. rhs)
    | _ -> 
            printf "Invalid Mul: lhs = %s, rhs = %s\n" (string_of_val ss lhs) (string_of_val ss rhs);
            assert false

let val_div lhs rhs _ss = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs /. rhs)
    | _ -> assert false

let val_is_true v _ss = match v with
    | Boolean true -> true
    | _ -> false

let rec val_eq lhs rhs ss = match lhs, rhs with
    | Number lhs, Number rhs -> Boolean (Float.equal lhs rhs)
    | Boolean lhs, Boolean rhs -> Boolean (Bool.equal lhs rhs)
    | (Tuple lhs, Tuple rhs)|(ValList lhs, ValList rhs) -> begin
        match List.zip lhs rhs with
            | Ok zipped ->  
                let res = List.for_all zipped ~f:(fun (a, b) -> val_is_true (val_eq a b ss) ss)
                in Boolean res
            | _ -> Boolean false
    end
    | Atom lhs, Atom rhs -> Boolean (Int.equal lhs rhs)
    | _ -> Boolean false

let val_eq_bool l r ss = val_is_true (val_eq l r ss) ss

let val_neq lhs rhs ss = Boolean (not (val_is_true (val_eq lhs rhs ss) ss))

let val_lt lhs rhs _ss = match lhs, rhs with
    | Number lhs, Number rhs -> Boolean (Float.compare lhs rhs < 0)
    | _ -> assert false

let val_gt lhs rhs _ss = match lhs, rhs with
    | Number lhs, Number rhs -> Boolean (Float.compare lhs rhs > 0)
    | _ -> assert false

let val_leq lhs rhs _ss = match lhs, rhs with
    | Number lhs, Number rhs -> Boolean (Float.compare lhs rhs <= 0)
    | _ -> assert false

let val_geq lhs rhs _ss = match lhs, rhs with
    | Number lhs, Number rhs -> Boolean (Float.compare lhs rhs >= 0)
    | _ -> assert false

let val_and lhs rhs _ss = match lhs, rhs with
    | Boolean lhs, Boolean rhs -> Boolean (lhs && rhs)
    | _ -> assert false

let val_or lhs rhs _ss = match lhs, rhs with
    | Boolean lhs, Boolean rhs -> Boolean (lhs || rhs)
    | _ -> assert false

let val_mod lhs rhs _ss = match lhs, rhs with
    | Number lhs, Number rhs -> Number (Float.mod_float lhs rhs)
    | _ -> assert false

let val_negate rhs _ss = match rhs with
    | Number rhs -> Number (~-.rhs)
    | _ -> assert false

let val_negate_bool rhs _ss = match rhs with
    | Boolean rhs -> Boolean (not rhs)
    | _ -> assert false

let val_list_head rhs ss = match rhs with
    | ValList (head::_) -> head
    | _ ->
        printf "Invalid Head: rhs = %s\n" (string_of_val ss rhs);
        assert false

let val_list_tail rhs ss = match rhs with
    | ValList (_::tail) -> ValList tail
    | _ ->
        printf "Invalid Tail: rhs = %s\n" (string_of_val ss rhs);
        assert false
