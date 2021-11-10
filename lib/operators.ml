open Types
open Stdio
open Base

let val_add lhs rhs ss _loc = match lhs, rhs with
    | Integer lhs, Integer rhs -> Integer (lhs + rhs)
    | Number lhs, Number rhs -> Number (lhs +. rhs)
    | Integer lhs, Number rhs -> Number ((Int.to_float lhs) +. rhs)
    | Number lhs, Integer rhs -> Number (lhs +. (Int.to_float rhs))
    | ValList lhs, ValList rhs ->  ValList (lhs @ rhs)
    | StringVal lhs, StringVal rhs ->  StringVal (lhs ^ rhs)
    | _ -> 
            printf "Invalid Add: lhs = %s, rhs = %s\n" (string_of_val ss lhs) (string_of_val ss rhs);
            assert false

let val_sub lhs rhs ss loc = match lhs, rhs with
    | Integer lhs, Integer rhs -> Integer (lhs - rhs)
    | Number lhs, Number rhs -> Number (lhs -. rhs)
    | Integer lhs, Number rhs -> Number ((Int.to_float lhs) -. rhs)
    | Number lhs, Integer rhs -> Number (lhs -. (Int.to_float rhs))
    | _ ->
        printf "Invalid sub at %s: lhs = %s, rhs = %s\n" 
            (location_to_string loc) (string_of_val ss lhs) (string_of_val ss rhs);
        Caml.exit 0

let val_mul lhs rhs ss _loc = match lhs, rhs with
    | Integer lhs, Integer rhs -> Integer (lhs * rhs)
    | Number lhs, Number rhs -> Number (lhs *. rhs)
    | Integer lhs, Number rhs -> Number ((Int.to_float lhs) *. rhs)
    | Number lhs, Integer rhs -> Number (lhs *. (Int.to_float rhs))
    | _ -> 
            printf "Invalid Mul: lhs = %s, rhs = %s\n" (string_of_val ss lhs) (string_of_val ss rhs);
            assert false

let val_div lhs rhs _ss _loc = match lhs, rhs with
    | Integer lhs, Integer rhs when Int.equal (lhs % rhs) 0 -> Integer (lhs / rhs)
    | Integer lhs, Integer rhs -> Number ((Float.of_int lhs) /. (Float.of_int rhs))
    | Number lhs, Number rhs -> Number (lhs /. rhs)
    | Integer lhs, Number rhs -> Number ((Int.to_float lhs) /. rhs)
    | Number lhs, Integer rhs -> Number (lhs /. (Int.to_float rhs))
    | _ -> assert false

let val_is_true v _ss _loc = match v with
    | Boolean true -> true
    | _ -> false

let rec val_eq lhs rhs ss loc = match lhs, rhs with
    | Integer lhs, Integer rhs -> Boolean (Int.equal lhs rhs)
    | Number lhs, Number rhs -> Boolean (Float.equal lhs rhs)
    | Boolean lhs, Boolean rhs -> Boolean (Bool.equal lhs rhs)
    | (Tuple lhs, Tuple rhs)|(ValList lhs, ValList rhs) -> begin
        match List.zip lhs rhs with
            | Ok zipped ->  
                let res = List.for_all zipped ~f:(fun (a, b) -> val_is_true (val_eq a b ss loc) ss loc)
                in Boolean res
            | _ -> Boolean false
    end
    | Atom lhs, Atom rhs -> Boolean (Int.equal lhs rhs)
    | StringVal lhs, StringVal rhs -> Boolean (String.equal lhs rhs)
    | _ -> Boolean false

let val_eq_bool l r ss loc = val_is_true (val_eq l r ss loc) ss loc

let val_neq lhs rhs ss loc = Boolean (not (val_is_true (val_eq lhs rhs ss loc) ss loc))

let val_lt lhs rhs ss loc = match lhs, rhs with
    | Integer lhs, Integer rhs -> Boolean (Int.compare lhs rhs < 0)
    | Number lhs, Number rhs -> Boolean (Float.compare lhs rhs < 0)
    | StringVal lhs, StringVal rhs -> Boolean (String.compare lhs rhs < 0)
    | _ ->
        printf "Invalid <: lhs = %s, rhs = %s, at %s\n" 
            (string_of_val ss lhs)
            (string_of_val ss rhs)
            (location_to_string loc);
        Caml.exit 0

let val_gt lhs rhs _ss _loc = match lhs, rhs with
    | Integer lhs, Integer rhs -> Boolean (Int.compare lhs rhs > 0)
    | Number lhs, Number rhs -> Boolean (Float.compare lhs rhs > 0)
    | StringVal lhs, StringVal rhs -> Boolean (String.compare lhs rhs > 0)
    | _ -> assert false

let val_leq lhs rhs _ss _loc = match lhs, rhs with
    | Integer lhs, Integer rhs -> Boolean (Int.compare lhs rhs <= 0)
    | Number lhs, Number rhs -> Boolean (Float.compare lhs rhs <= 0)
    | StringVal lhs, StringVal rhs -> Boolean (String.compare lhs rhs <= 0)
    | _ -> assert false

let val_geq lhs rhs _ss _loc = match lhs, rhs with
    | Integer lhs, Integer rhs -> Boolean (Int.compare lhs rhs >= 0)
    | Number lhs, Number rhs -> Boolean (Float.compare lhs rhs >= 0)
    | StringVal lhs, StringVal rhs -> Boolean (String.compare lhs rhs >= 0)
    | _ -> assert false

let val_and lhs rhs _ss _loc = match lhs, rhs with
    | Boolean lhs, Boolean rhs -> Boolean (lhs && rhs)
    | _ -> assert false

let val_or lhs rhs _ss _loc = match lhs, rhs with
    | Boolean lhs, Boolean rhs -> Boolean (lhs || rhs)
    | _ -> assert false

let val_mod lhs rhs _ss _loc = match lhs, rhs with
    | Integer lhs, Integer rhs -> Integer (lhs % rhs)
    | Number lhs, Number rhs -> Number (Float.mod_float lhs rhs)
    | _ -> assert false

let val_negate rhs _ss _loc = match rhs with
    | Integer rhs -> Integer (~-rhs)
    | Number rhs -> Number (~-.rhs)
    | _ -> assert false

let val_negate_bool rhs _ss _loc = match rhs with
    | Boolean rhs -> Boolean (not rhs)
    | _ -> assert false

let val_list_head rhs ss _loc = match rhs with
    | ValList (head::_) -> head
    | _ ->
        printf "Invalid Head: rhs = %s\n" (string_of_val ss rhs);
        assert false

let val_list_tail rhs ss loc = match rhs with
    | ValList (_::tail) -> ValList tail
    | _ ->
        printf "Invalid Tail at %s: rhs = %s\n" (location_to_string loc) (string_of_val ss rhs);
        print_traceback ss;
        Caml.exit 0
