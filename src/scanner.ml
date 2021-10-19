open Base
open Stdio

type token =
    | True
    | False
    | Number of float
    | Ident of string
    | Operator of Types.operator
    | Let
    | Equal
    | LParen
    | RParen
    | Fn
    | If
    | Then
    | Else
    | Arrow
    | Comma;;

let is_numeric d = Base.Char.is_digit d || phys_equal d '.';;
let is_identic c = Base.Char.is_alphanum c || phys_equal c '_';;

let rec scan_digit ls =
    let rec aux ls acc = match ls with
        | d::xs when is_numeric d -> aux xs (d::acc)
        | _ -> let f = (acc |> List.rev |> String.of_char_list |> Float.of_string)
                in (Number f)::(scan_ls ls)
    in aux ls []

and scan_ident ls =
    let rec aux ls acc = match ls with
        | c::xs when is_identic c -> aux xs (c::acc)
        | _ -> let n = (acc |> List.rev |> String.of_char_list)
                in (Ident n)::(scan_ls ls)
    in aux ls []

and scan_ls = function
    | [] -> []
    | ' '::xs -> scan_ls xs
    | '\t'::xs -> scan_ls xs
    | '='::'>'::xs -> Arrow :: scan_ls xs
    | '+'::xs -> Operator Add :: (scan_ls xs)
    | '-'::xs -> Operator Sub :: scan_ls xs
    | '*'::xs -> Operator Mul :: scan_ls xs
    | '/'::xs -> Operator Div :: scan_ls xs
    | '<'::xs -> Operator LT :: scan_ls xs
    | '>'::xs -> Operator GT :: scan_ls xs
    | '='::'='::xs -> Operator EQ :: scan_ls xs
    | '('::xs -> LParen :: scan_ls xs
    | ')'::xs -> RParen :: scan_ls xs
    | '='::xs -> Equal :: scan_ls xs
    | ','::xs -> Comma :: scan_ls xs
    | 'l'::'e'::'t'::xs -> Let :: scan_ls xs
    | 'f'::'n'::xs -> Fn :: scan_ls xs
    | 'i'::'f'::xs -> If :: scan_ls xs
    | 't'::'h'::'e'::'n'::xs -> Then :: scan_ls xs
    | 'e'::'l'::'s'::'e'::xs -> Else :: scan_ls xs
    | 'T'::xs -> True :: scan_ls xs
    | 'F'::xs -> False :: scan_ls xs
    | d::_ as ls when Base.Char.is_digit d -> scan_digit ls
    | i::_ as ls when not (Base.Char.is_digit i) -> scan_ident ls
    | _ as ls -> 
            printf "Scan Error: %s\n" (String.of_char_list ls); 
            assert false;;

let scan s = s |> String.to_list |> scan_ls;;

let string_of_tok = function
    | Number f -> Float.to_string f
    | Ident s -> "(Ident " ^ s ^ ")"
    | Operator _ -> "Operator"
    | Let -> "Let"
    | Equal -> "Equal"
    | LParen -> "LParen"
    | RParen -> "RParen"
    | Comma -> "Comma"
    | Fn -> "Fn"
    | Arrow -> "Arrow"
    | True -> "True"
    | False -> "False"
    | If -> "If"
    | Then -> "Then"
    | Else -> "Else"

let print_toks ls =
    List.iter ~f:(fun t -> printf "%s " (string_of_tok t)) ls;
    printf "\n";;
