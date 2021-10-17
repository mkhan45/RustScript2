open Printf

type token =
    | Number of float
    | Ident of string
    | Operator of Core.operator
    | Let
    | Equal
    | LParen
    | RParen;;

let is_numeric d = Base.Char.is_digit d || d == '.';;
let is_identic c = Base.Char.is_alphanum c;;

let chars_of_string s = List.init (String.length s) (String.get s);;
let rec string_of_chars chars = 
    let buf = Buffer.create 16 
        in List.iter (Buffer.add_char buf) chars;
    Buffer.contents buf

and scan_digit ls =
    let rec aux ls acc = match ls with
        | d::xs when is_numeric d -> aux xs (d::acc)
        | _ -> let f = (acc |> List.rev |> string_of_chars |> float_of_string)
                in (Number f)::(scan_ls ls)
    in aux ls []

and scan_ident ls =
    let rec aux ls acc = match ls with
        | c::xs when is_identic c -> aux xs (c::acc)
        | _ -> let n = (acc |> List.rev |> string_of_chars)
                in (Ident n)::(scan_ls ls)
    in aux ls []

and scan_ls = function
    | [] -> []
    | ' '::xs -> scan_ls xs
    | '\t'::xs -> scan_ls xs
    | '+'::xs -> Operator Add :: (scan_ls xs)
    | '-'::xs -> Operator Sub :: scan_ls xs
    | '*'::xs -> Operator Mul :: scan_ls xs
    | '/'::xs -> Operator Div :: scan_ls xs
    | '('::xs -> LParen :: scan_ls xs
    | ')'::xs -> RParen :: scan_ls xs
    | '='::xs -> Equal :: scan_ls xs
    | 'l'::'e'::'t'::xs -> Let :: scan_ls xs
    | d::_ as ls when Base.Char.is_digit d -> scan_digit ls
    | i::_ as ls when not (Base.Char.is_digit i) -> scan_ident ls
    | _ as ls -> 
            printf "Scan Error: %s\n" (string_of_chars ls); 
            assert false;;

let scan s = s |> chars_of_string |> scan_ls;;
