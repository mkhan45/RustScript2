open Base
open Stdio
open Printf
open Types

type token =
    | True
    | False
    | Number of float
    | Integer of int
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

let rec scan_digit ls line_num filename =
    let rec aux ls acc saw_dot = match ls with
        | '.'::_ when saw_dot -> begin match acc with
            | '.'::chars -> chars, scan_ls ('.'::ls) line_num filename, false
            | _ -> acc, scan_ls ls line_num filename, true
        end
        | '.'::xs -> aux xs ('.'::acc) true
        | d::xs when (is_numeric d) -> aux xs (d::acc) saw_dot
        | _ -> acc, scan_ls ls line_num filename, saw_dot
    in 
    let chars, scanned, saw_dot = aux ls [] false in
    if saw_dot then
        let f = chars |> List.rev |> String.of_char_list |> Float.of_string in
        (Number f |> Located.locate {line_num; filename})::scanned
    else
        let i = chars |> List.rev |> String.of_char_list |> Int.of_string in
        (Integer i |> Located.locate {line_num; filename})::scanned

and scan_ident ls line_num filename =
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
                (tok |> Located.locate {line_num; filename})::(scan_ls ls line_num filename)
    in aux ls []

and scan_string ls line_num filename =
    let rec aux ls acc = match ls with
        | '"'::xs -> 
            (StringTok (String.of_char_list (List.rev acc)) 
            |> Located.locate {line_num; filename})::(scan_ls xs line_num filename)
        | '\\'::'"'::xs -> aux xs ('"'::acc)
        | c::xs -> aux xs (c::acc)
        | [] ->
            printf "Unmatched quote";
            assert false
    in aux ls []

and skip_until_newline = function
    | [] -> []
    | '\n'::xs -> xs
    | _::xs -> skip_until_newline xs

and scan_ls ls line filename =
    let locate = Located.locate {line_num = line; filename = filename} in
    match ls with
    | [] -> []
    | (' '|'\t')::xs -> scan_ls xs line filename
    | '\n'::xs -> 
        (Newline |> locate) :: scan_ls xs (line + 1) filename
    | '='::'>'::xs -> 
        (Arrow |> locate):: scan_ls xs line filename
    | '-'::'>'::xs -> 
        (MatchArrow |> locate) :: scan_ls xs line filename
    | '+'::xs -> 
        (Operator Add |> locate) :: scan_ls xs line filename
    | '-'::xs -> 
        (Operator Neg |> locate) :: scan_ls xs line filename
    | '*'::xs -> 
        (Operator Mul |> locate) :: scan_ls xs line filename
    | '/'::xs -> 
        (Operator Div |> locate) :: scan_ls xs line filename
    | '<'::'='::xs -> 
        (Operator LEQ |> locate) :: scan_ls xs line filename
    | '<'::xs -> 
        (Operator LT |> locate) :: scan_ls xs line filename
    | '>'::'='::xs -> 
        (Operator GEQ |> locate) :: scan_ls xs line filename
    | '>'::xs -> 
        (Operator GT |> locate) :: scan_ls xs line filename
    | '&'::'&'::xs -> 
        (Operator And |> locate) :: scan_ls xs line filename
    | '|'::'|'::xs -> 
        (Operator Or |> locate) :: scan_ls xs line filename
    | '='::'='::xs -> 
        (Operator EQ |> locate) :: scan_ls xs line filename
    | '!'::'='::xs -> 
        (Operator NEQ |> locate) :: scan_ls xs line filename
    | '%'::xs -> 
        (Percent |> locate) :: scan_ls xs line filename
    | '^'::xs -> 
        (Operator Head |> locate) :: scan_ls xs line filename
    | '$'::xs -> 
        (Operator Tail |> locate) :: scan_ls xs line filename
    | '!'::xs -> 
        (Operator Not |> locate) :: scan_ls xs line filename
    | '.'::'.'::xs -> 
        (DotDot |> locate) :: scan_ls xs line filename
    | '('::xs -> 
        (LParen |> locate) :: scan_ls xs line filename
    | ')'::xs -> 
        (RParen |> locate) :: scan_ls xs line filename
    | '{'::xs -> 
        (LBrace |> locate) :: scan_ls xs line filename
    | '}'::xs -> 
        (RBrace |> locate) :: scan_ls xs line filename
    | '['::xs -> 
        (LBracket |> locate) :: scan_ls xs line filename
    | ']'::xs -> 
        (RBracket |> locate) :: scan_ls xs line filename
    | '='::xs -> 
        (Equal |> locate) :: scan_ls xs line filename
    | '_'::xs -> 
        (Underscore |> locate) :: scan_ls xs line filename
    | ','::xs -> 
        (Comma |> locate) :: scan_ls xs line filename
    | '#'::xs -> 
        scan_ls (skip_until_newline xs) (line + 1) filename
    | '|'::'>'::xs -> 
        ((Operator PipeOp) |> locate) :: scan_ls xs line filename
    | '|'::xs -> 
        (Pipe |> locate) :: scan_ls xs line filename
    | 'T'::xs -> 
        (True |> locate) :: scan_ls xs line filename
    | 'F'::xs -> 
        (False |> locate) :: scan_ls xs line filename
    | ':'::xs -> 
        (Colon |> locate) :: scan_ls xs line filename
    | '"'::xs -> scan_string xs line filename
    | d::_ as ls when Char.is_digit d -> scan_digit ls line filename
    | i::_ as ls when Char.is_alpha i -> scan_ident ls line filename
    | ls -> 
            printf "Scan Error: %s\n" (String.of_char_list ls); 
            assert false

let scan s ~filename = s |> String.to_list |> (fun s -> scan_ls s 1 filename)

let string_of_tok = function
    | Number f -> Float.to_string f
    | Integer i -> Int.to_string i
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
