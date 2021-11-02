open Base
open Stdio
open Printf

module Located = struct
    type 'a t = { line_num: int; data: 'a }

    let locate line_num a = { line_num; data = a }
    let extract {data; _} = data
    let location_to_string {line_num; _} = sprintf "line %s" (Int.to_string line_num)
end

type token =
    | True
    | False
    | Number of float
    | Ident of string
    | StringTok of string
    | Operator of Types.operator
    | Match
    | Let
    | Equal
    | LParen
    | RParen
    | LBrace
    | RBrace
    | LBracket
    | RBracket
    | Fn
    | When
    | If
    | Then
    | Else
    | Arrow
    | MatchArrow
    | Newline
    | Hashtag
    | Comma
    | Pipe
    | Underscore
    | Colon
    | Percent
    | DotDot
    | For
    | In
    | As

let is_numeric d = Base.Char.is_digit d
let is_identic c = Base.Char.is_alphanum c || phys_equal c '_'

let rec scan_digit ls line =
    let rec aux ls acc saw_dot = match ls with
        | '.'::_ when saw_dot -> begin match acc with
            | '.'::chars -> chars, scan_ls ('.'::ls) line
            | _ -> acc, scan_ls ls line
        end
        | '.'::xs -> aux xs ('.'::acc) true
        | d::xs when (is_numeric d) -> aux xs (d::acc) saw_dot
        | _ -> acc, scan_ls ls line
    in 
    let chars, scanned = aux ls [] false in
    let f = chars |> List.rev |> String.of_char_list |> Float.of_string in
    (Number f |> Located.locate line)::scanned

and scan_ident ls line =
    let rec aux ls acc = match ls with
        | c::xs when is_identic c -> aux xs (c::acc)
        | _ -> let n = (acc |> List.rev |> String.of_char_list) in
               let tok = match n with
                   | "let" -> Let
                   | "fn" -> Fn
                   | "if" -> If
                   | "then" -> Then
                   | "else" -> Else
                   | "match" -> Match
                   | "when" -> When
                   | "for" -> For
                   | "in" -> In
                   | "as" -> As
                   | _ -> Ident n
                in
                (tok |> Located.locate line)::(scan_ls ls line)
    in aux ls []

and scan_string ls line =
    let rec aux ls acc = match ls with
        | '"'::xs -> (StringTok (String.of_char_list (List.rev acc)) |> Located.locate line)::(scan_ls xs line)
        | c::xs -> aux xs (c::acc)
        | [] ->
            printf "Unmatched quote";
            assert false
    in aux ls []

and skip_until_newline = function
    | [] -> []
    | '\n'::xs -> xs
    | _::xs -> skip_until_newline xs

and scan_ls: char list -> int -> (token Located.t) list = fun ls line -> match ls with
    | [] -> []
    | (' '|'\t')::xs -> scan_ls xs line
    | '\n'::xs -> (Newline |> Located.locate line) :: scan_ls xs (line + 1)
    | '='::'>'::xs -> (Arrow |> Located.locate line):: scan_ls xs line
    | '-'::'>'::xs -> (MatchArrow |> Located.locate line) :: scan_ls xs line
    | '+'::xs -> (Operator Add |> Located.locate line) :: scan_ls xs line
    | '-'::xs -> (Operator Neg |> Located.locate line) :: scan_ls xs line
    | '*'::xs -> (Operator Mul |> Located.locate line) :: scan_ls xs line
    | '/'::xs -> (Operator Div |> Located.locate line) :: scan_ls xs line
    | '<'::'='::xs -> (Operator LEQ |> Located.locate line) :: scan_ls xs line
    | '<'::xs -> (Operator LT |> Located.locate line) :: scan_ls xs line
    | '>'::'='::xs -> (Operator GEQ |> Located.locate line) :: scan_ls xs line
    | '>'::xs -> (Operator GT |> Located.locate line) :: scan_ls xs line
    | '&'::'&'::xs -> (Operator And |> Located.locate line) :: scan_ls xs line
    | '|'::'|'::xs -> (Operator Or |> Located.locate line) :: scan_ls xs line
    | '='::'='::xs -> (Operator EQ |> Located.locate line) :: scan_ls xs line
    | '!'::'='::xs -> (Operator NEQ |> Located.locate line) :: scan_ls xs line
    | '%'::xs -> (Percent |> Located.locate line) :: scan_ls xs line
    | '^'::xs -> (Operator Head |> Located.locate line) :: scan_ls xs line
    | '$'::xs -> (Operator Tail |> Located.locate line) :: scan_ls xs line
    | '!'::xs -> (Operator Not |> Located.locate line) :: scan_ls xs line
    | '.'::'.'::xs -> (DotDot |> Located.locate line) :: scan_ls xs line
    | '('::xs -> (LParen |> Located.locate line) :: scan_ls xs line
    | ')'::xs -> (RParen |> Located.locate line) :: scan_ls xs line
    | '{'::xs -> (LBrace |> Located.locate line) :: scan_ls xs line
    | '}'::xs -> (RBrace |> Located.locate line) :: scan_ls xs line
    | '['::xs -> (LBracket |> Located.locate line) :: scan_ls xs line
    | ']'::xs -> (RBracket |> Located.locate line) :: scan_ls xs line
    | '='::xs -> (Equal |> Located.locate line) :: scan_ls xs line
    | '_'::xs -> (Underscore |> Located.locate line) :: scan_ls xs line
    | ','::xs -> (Comma |> Located.locate line) :: scan_ls xs line
    | '#'::xs -> scan_ls (skip_until_newline xs) (line + 1)
    | '|'::xs -> (Pipe |> Located.locate line) :: scan_ls xs line
    | 'T'::xs -> (True |> Located.locate line) :: scan_ls xs line
    | 'F'::xs -> (False |> Located.locate line) :: scan_ls xs line
    | ':'::xs -> (Colon |> Located.locate line) :: scan_ls xs line
    | '"'::xs -> scan_string xs line
    | d::_ as ls when Char.is_digit d -> scan_digit ls line
    | i::_ as ls when Char.is_alpha i -> scan_ident ls line
    | ls -> 
            printf "Scan Error: %s\n" (String.of_char_list ls); 
            assert false

let scan s = s |> String.to_list |> (fun s -> scan_ls s 1)

let string_of_tok = function
    | Number f -> Float.to_string f
    | Ident s -> "(Ident " ^ s ^ ")"
    | StringTok s -> sprintf "String (\"%s\")" s
    | Operator _ -> "Operator"
    | Let -> "Let"
    | Equal -> "Equal"
    | LParen -> "LParen"
    | RParen -> "RParen"
    | LBrace -> "LBrace"
    | RBrace -> "RBrace"
    | LBracket -> "LBracket"
    | RBracket -> "RBracket"
    | Comma -> "Comma"
    | Fn -> "Fn"
    | Arrow -> "Arrow"
    | True -> "True"
    | False -> "False"
    | When -> "When"
    | If -> "If"
    | Then -> "Then"
    | Else -> "Else"
    | Newline -> "Newline"
    | Hashtag -> "Hashtag"
    | Pipe -> "Pipe"
    | Match -> "Match"
    | MatchArrow -> "MatchArrow"
    | Underscore -> "Underscore"
    | Colon -> "Colon"
    | Percent -> "Percent"
    | DotDot -> "DotDot"
    | For -> "For"
    | In -> "In"
    | As -> "As"

let string_of_toks ls = String.concat ~sep:" " (List.map ~f:string_of_tok ls)
let print_toks ls = ls |> string_of_toks |> printf "%s\n"

let toks_empty toks = List.for_all toks ~f:(fun tok -> phys_equal tok Newline)
let rec skip_newlines = function
    | {Located.data = Newline; _} :: xs -> skip_newlines xs
    | ls -> ls
