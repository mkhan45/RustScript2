open Types
open Stdio
open Base
open Operators

(* Throughout, static state is abbreviated as ss *)

let rec bind: pattern -> value -> static_state -> location -> state -> state = fun lhs rhs ss loc ->
    let bind lhs rhs = bind lhs rhs ss loc in
    let pattern_matches lhs rhs = pattern_matches lhs rhs ss loc in
    (* printf "Binding %s to %s\n" (string_of_pat lhs) (string_of_val rhs); *)
    match lhs, rhs with
    | SinglePat (UnresolvedIdent s), _ -> fun _ ->
            printf "Error: found unresolved SinglePat %s at %s\n" s (location_to_string loc);
            print_traceback ss;
            Caml.exit 0;
    | SinglePat (ResolvedIdent i), _ -> fun state ->
            Map.set state ~key:i ~data:rhs
    | NumberPat lhs, Number rhs when Float.equal lhs rhs -> 
            fun state -> state
    | IntegerPat lhs, Integer rhs when Int.equal lhs rhs -> 
            fun state -> state
    | StringPat lhs, StringVal rhs when String.equal (escape_string lhs) (escape_string rhs) ->
            fun state -> state
    | AtomPat lhs, Atom rhs when Int.equal lhs rhs ->
            fun state -> state
    | OrPat (l, r), _ -> fun state ->
            if (pattern_matches l rhs state) then (bind l rhs) state else (bind r rhs) state
    | AsPat (pat, n), _ -> fun state ->
            let ident_id = List.Assoc.find_exn ss.static_idents ~equal:String.equal n in
            let state = bind (SinglePat (ResolvedIdent ident_id)) rhs state in
            bind pat rhs state
    | ((TuplePat lhs_ls) as lhs, ((Tuple rhs_ls) as rhs))|
      ((ListPat (FullPat lhs_ls)) as lhs, ((ValList rhs_ls) as rhs)) -> begin
          match List.zip lhs_ls rhs_ls with
            | Ok zipped ->
                fun state -> List.fold_left ~init:state ~f:(fun state (k, v) -> (bind k v) state) zipped
            | _ ->
                printf "\n";
                printf "Error at %s, Tried to bind %s of len %d to %s of len %d\n"
                    (location_to_string loc)
                    (string_of_pat ss lhs) (List.length lhs_ls)
                    (string_of_val ss rhs) (List.length rhs_ls);
                print_traceback ss;
                Caml.exit 0
      end
    | (ListPat (HeadTailPat (head_pat_ls, tail_pat))), ValList rhs_ls -> fun s ->
            let (head_ls, tail_ls) = List.split_n rhs_ls (List.length head_pat_ls) in
            let s = (bind (ListPat (FullPat head_pat_ls)) (ValList head_ls)) s in
            let s = (bind tail_pat (ValList tail_ls)) s in
            s
    | MapPat kv_pairs, Dictionary rhs -> fun s ->
        let fetched_pairs = kv_pairs
            |> List.map ~f:(fun (k, v) -> let ev_k, _ = (eval_expr k ss) s in ev_k, v)
            |> List.map ~f:(fun (k, v) -> dict_get rhs k ss loc, v)
        in
        let fold_step state (k, v) = (bind v k) state in
        List.fold_left ~init:s ~f:fold_step fetched_pairs
    | WildcardPat, _ -> fun state -> state
    | _ -> 
        printf "Error at %s, Tried to bind %s to %s\n"
            (location_to_string loc)
            (string_of_pat ss lhs)
            (string_of_val ss rhs);
        print_traceback ss;
        Caml.exit 0

and dict_get dict key ss loc =
    (* Can probably be replaced by Base.Option functions *)
    match Map.find dict (hash_value key) with
        | Some found_values ->
            let res = List.Assoc.find found_values ~equal:(fun a b -> val_eq_bool a b ss loc) key in
            Option.value ~default:(Tuple []) res
        | _ -> Tuple []

and list_equal_len lhs rhs = match lhs, rhs with
    | [], [] -> true
    | [], _ | _, [] -> false
    | _::xs, _::ys -> list_equal_len xs ys

and pattern_matches: pattern -> value -> static_state -> location -> state -> bool = fun pat value ss loc state ->
    let pattern_matches pat value = pattern_matches pat value ss loc in
    let eval_expr expr ?tc:(tc=false) = eval_expr expr ss ~tc:tc in
    match pat, value with
        | WildcardPat, _ -> true
        | SinglePat _, _ -> true
        | AsPat (pat, _), _ -> pattern_matches pat value state
        | OrPat (lhs, rhs), value -> (pattern_matches lhs value state) || (pattern_matches rhs value state)
        | NumberPat lhs, Number rhs -> Float.equal lhs rhs
        | IntegerPat lhs, Integer rhs -> Int.equal lhs rhs
        | StringPat lhs, StringVal rhs -> String.equal (escape_string lhs) (escape_string rhs)
        | AtomPat lhs, Atom rhs -> Int.equal lhs rhs
        | ((TuplePat lhs_ls), (Tuple rhs_ls))|(ListPat (FullPat lhs_ls), ValList rhs_ls) ->
            if list_equal_len lhs_ls rhs_ls then
                let zipped = List.zip_exn lhs_ls rhs_ls in
                List.for_all ~f:(fun (p, v) -> pattern_matches p v state) zipped
            else false
        | (ListPat (HeadTailPat (head_pat_ls, tail_pat)), ValList rhs_ls) ->
            let (head_ls, tail_ls) = List.split_n rhs_ls (List.length head_pat_ls) in
            let head_matches = pattern_matches (ListPat (FullPat head_pat_ls)) (ValList head_ls) state in
            let tail_matches = pattern_matches tail_pat (ValList tail_ls) state in
            head_matches && tail_matches
        | (MapPat kv_pairs, Dictionary rhs) ->
            let fetched_pairs = kv_pairs
                |> List.map ~f:(fun (k, v) -> let ev_k, _ = (eval_expr k) state in ev_k, v)
                |> List.map ~f:(fun (k, v) -> dict_get rhs k ss loc, v)
            in
            List.for_all ~f:(fun (k, v) -> pattern_matches v k state) fetched_pairs
        | _ -> false

and inspect_builtin (args, state) ss =
    match args with
        | Tuple [v] ->
            printf "%s\n" (string_of_val ss v);
            Stdio.Out_channel.flush Stdio.stdout;
            v, state
        | _ ->
            printf "Expected only one argument to inspect";
            assert false

and print_builtin (args, state) ss loc =
    match args with
        | Tuple [StringVal s] ->
            s |> escape_string |> printf "%s";
            Stdio.Out_channel.flush Stdio.stdout;
            (Tuple [], state)
        | _ ->
            printf "Expected one string argument to print at %s\n" (location_to_string loc);
            print_traceback ss;
            Caml.exit 0

(* Better as a builtin since concatenating strings is expensive *)
and println_builtin (args, state) ss loc =
    match args with
        | Tuple [StringVal s] ->
            s |> escape_string |> printf "%s\n";
            Stdio.Out_channel.flush Stdio.stdout;
            (Tuple [], state)
        | _ ->
            printf "Expected one string argument to println at %s\n" (location_to_string loc);
            print_traceback ss;
            Caml.exit 0

and to_string_builtin (args, state) ss loc =
    match args with
        | Tuple [v] ->
            (StringVal (string_of_val ss v)), state
        | _ ->
            printf "Expected only one argument to to_string at %s\n" (location_to_string loc);
            print_traceback ss;
            Caml.exit 0

and string_to_num_builtin (args, state) ss loc = 
    match args with
        | Tuple [StringVal s] -> begin
            try
                Tuple [Atom 0; Number (Float.of_string s)], state
            with
                | _ -> Atom 1, state
        end
        | _ ->
            printf "Expected one string argument to string_to_num at %s\n" (location_to_string loc);
            print_traceback ss;
            Caml.exit 0

and string_to_int_builtin (args, state) ss loc = 
    match args with
        | Tuple [StringVal s] -> begin
            try
                Tuple [Atom 0; Integer (Int.of_string s)], state
            with
                | _ -> Atom 1, state
        end
        | _ ->
            printf "Expected one string argument to string_to_num at %s\n" (location_to_string loc);
            print_traceback ss;
            Caml.exit 0

and scanln_builtin (args, state) ss loc =
    match args with
        | Tuple [] -> begin
            match Stdio.In_channel.input_line ~fix_win_eol:true Stdio.stdin with
            | Some line -> (StringVal line), state
            | None -> Tuple [], state
        end
        | _ ->
            printf "Expected () as an argument to scan_line at %s\n" (location_to_string loc);
            print_traceback ss;
            Caml.exit 0

and range_builtin (args, state) ss loc =
    match args with
        | Tuple [Integer start; Integer end_; Integer step] ->
            let caml_ls = List.range start end_ ~stride:step in
            let val_ls = List.map ~f:(fun n -> Integer n) caml_ls in
            ValList val_ls, state
        | _ ->
            printf "Expected three integer arguments to range_step at %s, got %s\n" 
                (location_to_string loc)
                (string_of_val ss args);
            print_traceback ss;
            Caml.exit 0

and fold_builtin (args, state) ss loc =
    match args with
        | Tuple [init; Lambda fn; ValList ls] ->
            let call_fn = fun args ->
                let lambda_call = Thunk {thunk_fn = fn; thunk_args= args; thunk_fn_name = ResolvedIdent 0} in
                let res, _ = unwrap_thunk lambda_call state ss loc in
                res
            in
            let fold_result = 
                List.fold 
                ~init:init
                ~f:(fun acc v -> call_fn (Tuple [acc; v]))
                ls
            in
            fold_result, state
        | Tuple [init; Fn fn; ValList ls] ->
            let call_fn = fun args ->
                let block_funcs = List.Assoc.map ss.static_block_funcs ~f:(fun f -> Fn f) in
                let enclosed_state = Map.of_alist_reduce (module Int) block_funcs ~f:(fun a _ -> a) in
                let pseudo_lambda = {lambda_expr = fn.fn_expr; lambda_args = fn.fn_args; enclosed_state } in
                let lambda_call = Thunk {thunk_fn = pseudo_lambda; thunk_args= args; thunk_fn_name = ResolvedIdent 0} in
                let res, _ = unwrap_thunk lambda_call state ss loc in
                res
            in
            let fold_result = 
                List.fold 
                ~init:init
                ~f:(fun acc v -> call_fn (Tuple [acc; v]))
                ls
            in
            fold_result, state
        | _ ->
            printf "Expected (init, fn, ls) as arguments to fold at %s, got %s\n" 
                (location_to_string loc)
                (string_of_val ss args);
            print_traceback ss;
            Caml.exit 0

and to_charlist_builtin (args, state) _ss =
    match args with
        | Tuple [StringVal s] ->
            let chars = String.to_list s in
            let char_strs = List.map ~f:String.of_char chars in
            let val_ls = List.map ~f:(fun c -> StringVal c) char_strs in
            ValList val_ls, state
        | _ ->
            printf "Expected a single string argument to to_charlist";
            assert false

and get_builtin (args, state) ss loc = 
    match args with
        | Tuple [Dictionary m; key] -> begin
            match Map.find m (hash_value key) with
                | Some found_values -> 
                    let res = 
                        List.Assoc.find found_values ~equal:(fun a b -> val_eq_bool a b ss loc) key 
                    in
                    let v = Option.value ~default:(Tuple []) res in
                    v, state
                | None -> (Tuple [], state)
            end
        | _ ->
            printf "get requires two arguments, a list, and a value";
            assert false

and read_file_builtin (args, state) _ss _loc =
    match args with
        | Tuple [StringVal filename] -> begin
            try
                let in_stream = In_channel.create filename in
                let file_string = In_channel.input_all in_stream in
                In_channel.close in_stream;
                StringVal file_string, state
            with Sys_error err_str ->
                Tuple [Atom 1; StringVal err_str], state
        end
        | _ -> assert false

and write_file_builtin (args, state) ss loc =
    match args with
        | Tuple [StringVal filename; StringVal data] -> begin
            try
                let out_stream = Out_channel.create filename in
                Out_channel.output_string out_stream (escape_string data);
                Out_channel.close out_stream;
                Tuple [], state
            with Sys_error err_str ->
                printf "Error at %s: %s\n" (location_to_string loc) err_str;
                print_traceback ss;
                Caml.exit 0
        end
        | _ -> assert false

and list_dir_builtin (args, state) ss loc =
    match args with
        | Tuple [StringVal dirname] -> begin
            try
                let filenames = Caml.Sys.readdir dirname
                    |> Array.to_list
                    |> List.map ~f:(fun n -> StringVal n)
                in
                ValList filenames, state
            with Sys_error err_str ->
                printf "Error at %s: %s\n" (location_to_string loc) err_str;
                print_traceback ss;
                Caml.exit 0
        end
        | _ -> assert false

and mkdir_builtin (args, state) _ss _loc =
    match args with
        | Tuple [StringVal dirname] -> begin
            try
                match Caml.Sys.command (Printf.sprintf "mkdir %s" dirname) with
                    | 0 -> Atom 0, state
                    | _ -> Tuple [Atom 1; StringVal "Nonzero exit code"], state
            with Sys_error err_str ->
                Tuple [Atom 1; StringVal err_str], state
        end
        | _ -> assert false

and map_keys_builtin (args, state) _ss _loc =
    match args with
        | Tuple [Dictionary dict] ->
            let keys = dict 
                |> Map.data 
                |> List.map ~f:(fun assoc_list -> List.map ~f:(fun (k, _) -> k) assoc_list)
                |> List.concat_no_order
            in
            ValList keys, state
        | _ -> assert false

and map_to_list_builtin (args, state) _ss _loc =
    match args with
        | Tuple [Dictionary dict] ->
            let ls = dict 
                |> Map.data 
                |> List.concat
                |> List.rev
                |> List.map ~f:(fun (k, v) -> Tuple [k; v])
            in
            ValList ls, state
        | _ -> assert false

and typeof_builtin (args, state) _ss _loc =
    match args with
        | Tuple [Number _] -> Atom 2, state
        | Tuple [Integer _] -> Atom 3, state
        | Tuple [Boolean _] -> Atom 4, state
        | Tuple [Tuple _] -> Atom 5, state
        | Tuple [ValList _] -> Atom 6, state
        | Tuple [(Lambda _) | (LambdaCapture _) | (Fn _)] -> Atom 7, state
        | Tuple [Dictionary _] -> Atom 8, state
        | Tuple [Atom _] -> Atom 9, state
        | Tuple [StringVal _] -> Atom 10, state
        | _ -> assert false

and serve_builtin (args, interpreter_state) ss loc =
    match args with
        | Tuple [Integer port; Lambda lambda; server_state] ->
            let open Lwt in
            let open Cohttp in
            let open Cohttp_lwt_unix in
            
            let server_ref = ref server_state in

            let callback _conn req body =
            let uri = req |> Request.uri |> Uri.to_string in
            let meth = req |> Request.meth |> Code.string_of_method in
            let headers = req |> Request.headers |> Header.to_string in
            ( body |> Cohttp_lwt.Body.to_string >|= fun body -> body )
            >>= fun body -> 
                    let args = Tuple [StringVal uri; StringVal meth; StringVal headers; StringVal body; !server_ref] in
                    let thunk = Thunk {thunk_fn = lambda; thunk_args = args; thunk_fn_name = ResolvedIdent ~-1} in
                    let res, headers = match unwrap_thunk thunk interpreter_state ss loc with
                        | Tuple [StringVal s; server_state; Dictionary headers], _ -> 
                            server_ref := server_state;
                            let header_pairs = List.concat (Map.data headers) in
                            let header_pairs = List.map header_pairs ~f:(fun (k, v) -> match k, v with
                                | StringVal k, StringVal v -> k, v
                                | _ -> assert false
                            )
                            in
                            let headers = Header.add_list (Header.init ()) header_pairs in
                            s, headers
                        | StringVal s, _ -> 
                            s, Header.init ()
                        | _ -> assert false
                    in
                    Server.respond_string ~status:`OK ~headers ~body:res ()
            in
            Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())
        | _ ->
            assert false

and serve_ssl_builtin (args, interpreter_state) ss loc =
    match args with
        | Tuple [StringVal cert_path; StringVal key_path; Integer port; Lambda lambda; server_state] ->
            let open Lwt in
            let open Cohttp in
            let open Cohttp_lwt_unix in

            let server_ref = ref server_state in

            let callback _conn req body =
            let uri = req |> Request.uri |> Uri.to_string in
            let meth = req |> Request.meth |> Code.string_of_method in
            let headers = req |> Request.headers |> Header.to_string in
            ( body |> Cohttp_lwt.Body.to_string >|= fun body -> body )
            >>= fun body -> 
                    let args = Tuple [StringVal uri; StringVal meth; StringVal headers; StringVal body; !server_ref] in
                    let thunk = Thunk {thunk_fn = lambda; thunk_args = args; thunk_fn_name = ResolvedIdent ~-1} in
                    let res = match unwrap_thunk thunk interpreter_state ss loc with
                        | Tuple [StringVal s; server_state], _ ->
                            server_ref := server_state;
                            s
                        | StringVal s, _ -> 
                            s
                        | _ -> assert false
                    in
                    Server.respond_string ~status:`OK ~body:res ()
            in
            let tls_config = `Crt_file_path cert_path, `Key_file_path key_path, `No_password, `Port port in
            Server.create ~mode:(`TLS tls_config) (Server.make ~callback ())
        | _ ->
            assert false

and crypto_hash_builtin (args, state) _ss _loc =
    match args with
        | Tuple [StringVal s] -> StringVal (Bcrypt.string_of_hash (Bcrypt.hash s)), state
        | _ -> assert false

and validate_pass_builtin (args, state) _ss _loc =
    match args with
        | Tuple [StringVal a; StringVal b] -> Boolean (Bcrypt.verify a (Bcrypt.hash_of_string b)), state
        | _ -> assert false

and truncate_builtin (args, state) _ss _loc =
    match args with
        | Tuple [Number n] -> Integer (Int.of_float n), state
        | _ -> assert false

and eval_pipe ~tc lhs rhs ss loc = fun s ->
    let {Located.location = args_loc; _} = lhs in
    let {Located.location = fn_loc; _} = rhs in
    let (lhs, s) = (eval_expr lhs ss) s in
    let (rhs, s) = (eval_expr rhs ss) s in
    match rhs with
    | Lambda _ | Fn _ | LambdaCapture _ | Dictionary _ ->
        let call_args =
            (TupleExpr ([Atomic lhs |> Located.locate args_loc])) |> Located.locate args_loc
        in
        let call =
            {callee = ResolvedIdent ~-1; call_args}
        in
        eval_lambda ~tc:tc rhs call ss fn_loc s
    | _ ->
        printf "Tried to pipe to a non function %s at %s\n"
            (string_of_val ss rhs)
            (location_to_string loc);
        Caml.exit 0

and eval_op op lhs rhs ss loc = fun s ->
    let (lhs, s) = (eval_expr lhs ss) s in
    let (rhs, s) = (eval_expr rhs ss) s in
    op lhs rhs ss loc, s
    
and eval_prefix_op op rhs ss loc = fun s ->
    let (rhs, s) = (eval_expr rhs ss) s in
    op rhs ss loc, s

and eval_ident ss name loc = fun state ->
    match Map.find state name with
        | Some value -> value, state
        | None ->
            printf "Error at %s: variable not found: %s\n" 
                (location_to_string loc) 
                (name |> List.Assoc.find_exn (List.Assoc.inverse ss.static_idents) ~equal:Int.equal);
            print_traceback ss;
            Caml.exit 0

and eval_let lhs rhs ss loc = fun state ->
    let (evaled, new_state) = (eval_expr rhs ss) state in
    let new_state = (bind lhs evaled ss loc) new_state in
    (Tuple [], new_state)

and eval_fn_def name {fn_expr; fn_args} ss loc = fun state ->
    let fn = Fn {fn_expr; fn_args} in
    let new_state = (bind (SinglePat name) fn ss loc) state in
    (Tuple [], new_state)

and eval_lambda_def e args =
    fun s -> (Lambda {lambda_expr = e; lambda_args = args; enclosed_state = s}), s

and eval_lambda_capture capture ss loc state =
    let capture_callee_id = match capture.capture_expr_fn with
        | UnresolvedIdent s ->
            printf "Found unresolved ident %s at %s" s (location_to_string loc);
            Caml.exit 0
        | ResolvedIdent id -> id
    in
    let capture_val = match Map.find state capture_callee_id with
        | None ->
            begin match List.Assoc.find ss.static_block_funcs capture_callee_id ~equal:Int.equal with
            | Some f -> Fn f
            | None ->
                printf "Tried to make function capture out of nonexistent %s at %s\n"
                    (List.Assoc.find_exn (List.Assoc.inverse ss.static_idents) ~equal:Int.equal capture_callee_id)
                    (location_to_string loc);
                Caml.exit 0
            end
        | Some v -> v
    in
    let fold_step state arg = match arg with
        | CaptureExprArg e ->
            let evaled, state = (eval_expr e ss) state in
            state, ValArg evaled
        | BlankCaptureExprHole ->
            state, BlankCaptureHole
        | LabeledCaptureExprHole i ->
            state, LabeledCaptureHole i
    in
    let state, capture_args = List.fold_map capture.capture_expr_args ~init:state ~f:fold_step in
    let capture = LambdaCapture {capture_val; capture_args} in
    (capture, state)
                
and unwrap_thunk thunk state ss loc = match thunk with
    | Thunk {thunk_fn; thunk_args; thunk_fn_name = ResolvedIdent thunk_fn_name_id} ->
            let inner_state = (bind thunk_fn.lambda_args thunk_args ss loc) thunk_fn.enclosed_state in
            let inner_state = Map.set inner_state ~key:thunk_fn_name_id ~data:(Lambda thunk_fn) in
            let call_stack = match ss.call_stack with
                | ((id, loc), n)::xs when Int.equal id thunk_fn_name_id -> ((id, loc), n + 1)::xs
                | _ -> ((thunk_fn_name_id, loc), 1)::ss.call_stack
            in
            let ss = { ss with call_stack } in
            let (new_thunk, _) = (eval_expr ~tc:true thunk_fn.lambda_expr ss) inner_state in
            unwrap_thunk new_thunk state ss loc
    | Thunk {thunk_fn_name = UnresolvedIdent n; _} ->
            printf "Error: found unresolved ident %s at %s\n" n (location_to_string loc);
            print_traceback ss;
            Caml.exit 0
    | value -> value, state

and eval_lambda ~tc lambda call ss loc state = match lambda with
    | Lambda lambda_val ->
        let (evaled, _) = (eval_expr call.call_args ss) state in
        let thunk = Thunk {thunk_fn = lambda_val; thunk_args = evaled; thunk_fn_name = call.callee} in
        if tc then 
            (thunk, state)
        else 
            let res, _ = unwrap_thunk thunk state ss loc in
            (res, state)
    | Fn fn_val ->
        let (evaled, _) = (eval_expr call.call_args ss) state in
        let block_funcs = List.Assoc.map ss.static_block_funcs ~f:(fun f -> Fn f) in
        let enclosed_state = Map.of_alist_reduce (module Int) block_funcs ~f:(fun a _ -> a) in
        let pseudo_lambda = 
            {lambda_expr = fn_val.fn_expr; lambda_args = fn_val.fn_args; enclosed_state }
        in
        let thunk = Thunk {thunk_fn = pseudo_lambda; thunk_args = evaled; thunk_fn_name = call.callee } in
        if tc then
            (thunk, state)
        else
            let res, _ = unwrap_thunk thunk state ss loc in
            (res, state)
    | LambdaCapture capture ->
        let rec construct_arglist captured arglist call_args used_hole = match captured, call_args, used_hole with
            | [], [], _ | [], _, (Some LabeledHole) | [], _::_, None -> 
                List.rev arglist

            | [], _, (Some BlankHole) -> 
                printf "Called captured fn with too many arguments at %s\n" (location_to_string loc);
                Caml.exit 0

            | BlankCaptureHole::_, [], ((Some BlankHole)|None) -> 
                printf "Called captured fn with too few arguments at %s\n" (location_to_string loc);
                Caml.exit 0

            | (ValArg v)::xs, _, _ -> 
                let arg = (Atomic v) |> Located.locate loc in
                construct_arglist xs (arg::arglist) call_args used_hole

            | BlankCaptureHole::xs, arg::call_args, ((Some BlankHole)|None) -> 
                construct_arglist xs (arg::arglist) call_args (Some BlankHole)

            | (LabeledCaptureHole i)::xs, _, ((Some LabeledHole)|None) ->
                construct_arglist xs ((List.nth_exn call_args i)::arglist) call_args (Some LabeledHole)

            | BlankCaptureHole::_, _, (Some LabeledHole) 
            | (LabeledCaptureHole _)::_, _, (Some BlankHole) -> 
                printf "Tried to mix labeled and blank capture holes in call at %s\n"
                    (location_to_string loc);
                Caml.exit 0
        in
        let call_args = match call.call_args.data with
            | TupleExpr ls -> ls
            | _ -> assert false
        in
        let arglist = construct_arglist capture.capture_args [] call_args None in
        let call = {call with call_args = (TupleExpr arglist) |> Located.locate loc} in
        let call_stack = match ss.call_stack with
            | ((id, loc), n)::xs when Int.equal id ~-1 -> ((-1, loc), n + 1)::xs
            | _ -> ((-1, loc), 1)::ss.call_stack
        in
        let ss = { ss with call_stack } in
        eval_lambda ~tc:tc capture.capture_val call ss loc state
    | Dictionary dict ->
        let (evaled, state) = (eval_expr call.call_args ss) state in begin
            match evaled with
                | Tuple [key] -> dict_get dict key ss loc, state
                | _ ->
                    printf "Expected a single key\n";
                    assert false
        end
    | _ -> assert false

and eval_lambda_call ?tc:(tail_call=false) call ss loc =
    let callee_id = match call.callee with
        | UnresolvedIdent n -> List.Assoc.find_exn ss.static_idents ~equal:String.equal n
        | ResolvedIdent i -> i
    in
    fun (state: state) -> match Map.find state callee_id with
        | Some((Lambda _ | Fn _ | LambdaCapture _ | Dictionary _) as l) ->
            eval_lambda ~tc:tail_call l call ss loc state
        | None -> begin
            match call.callee with
                | ResolvedIdent 0 -> inspect_builtin ((eval_expr call.call_args ss) state) ss
                | ResolvedIdent 1 -> print_builtin ((eval_expr call.call_args ss) state) ss loc
                | ResolvedIdent 2 -> println_builtin ((eval_expr call.call_args ss) state) ss loc
                | ResolvedIdent 3 -> scanln_builtin ((eval_expr call.call_args ss) state) ss loc
                | ResolvedIdent 4 -> to_string_builtin ((eval_expr call.call_args ss) state) ss loc
                | ResolvedIdent 5 -> string_to_num_builtin ((eval_expr call.call_args ss) state) ss loc
                | ResolvedIdent 6 -> string_to_int_builtin ((eval_expr call.call_args ss) state) ss loc
                | ResolvedIdent 7 -> range_builtin ((eval_expr call.call_args ss) state) ss loc
                | ResolvedIdent 8 -> fold_builtin ((eval_expr call.call_args ss) state) ss loc
                | ResolvedIdent 9 -> to_charlist_builtin ((eval_expr call.call_args ss) state) ss
                | ResolvedIdent 10 -> get_builtin ((eval_expr call.call_args ss) state) ss loc
                | ResolvedIdent 11 -> read_file_builtin ((eval_expr call.call_args ss) state) ss loc
                | ResolvedIdent 12 -> write_file_builtin ((eval_expr call.call_args ss) state) ss loc
                | ResolvedIdent 13 -> list_dir_builtin ((eval_expr call.call_args ss) state) ss loc
                | ResolvedIdent 14 -> mkdir_builtin ((eval_expr call.call_args ss) state) ss loc
                | ResolvedIdent 15 -> map_keys_builtin ((eval_expr call.call_args ss) state) ss loc
                | ResolvedIdent 16 -> map_to_list_builtin ((eval_expr call.call_args ss) state) ss loc
                | ResolvedIdent 17 -> typeof_builtin ((eval_expr call.call_args ss) state) ss loc
                | ResolvedIdent 18 -> 
                        Lwt_main.run (serve_builtin ((eval_expr call.call_args ss) state) ss loc);
                        Tuple [], state
                | ResolvedIdent 19 -> 
                        Lwt_main.run (serve_ssl_builtin ((eval_expr call.call_args ss) state) ss loc);
                        Tuple [], state
                | ResolvedIdent 20 -> crypto_hash_builtin ((eval_expr call.call_args ss) state) ss loc
                | ResolvedIdent 21 -> validate_pass_builtin ((eval_expr call.call_args ss) state) ss loc
                | ResolvedIdent 22 -> truncate_builtin ((eval_expr call.call_args ss) state) ss loc
                | UnresolvedIdent s ->
                    printf "Error: unresolved function %s not found at %s\n" s (location_to_string loc);
                    print_traceback ss;
                    Caml.exit 0
                | ResolvedIdent i ->
                    let name = List.Assoc.find_exn (List.Assoc.inverse ss.static_idents) ~equal:Int.equal i in
                    printf "Error: resolved function %s (id %d) not found at %s\n" name i (location_to_string loc);
                    print_traceback ss;
                    Caml.exit 0
        end
        | _ -> match call.callee with
            | UnresolvedIdent s ->
                printf "Error: tried to call %s not found at %s\n" s (location_to_string loc);
                print_traceback ss;
                Caml.exit 0
            | ResolvedIdent i ->
                let name = List.Assoc.find_exn (List.Assoc.inverse ss.static_idents) ~equal:Int.equal i in
                printf "Error: tried to call %s not found at %s\n" name (location_to_string loc);
                print_traceback ss;
                Caml.exit 0

and eval_tuple_expr ls ss state =
    let (eval_ls, state) =
        List.fold_left 
            ~init:([], state) 
            ~f:(fun (acc, s) e -> let (ev, s) = (eval_expr e ss) s in (ev::acc, s))
            ls
    in
    Tuple (List.rev eval_ls), state

and eval_if_expr ?tc:(tail_call=false) if_expr ss = fun state ->
    match (eval_expr if_expr.cond ss) state with
        | Boolean true, state -> (eval_expr ~tc:tail_call if_expr.then_expr ss) state
        | Boolean false, state -> 
                (eval_expr ~tc:tail_call if_expr.else_expr ss) state
        | _ -> assert false

and eval_if_let_expr ?tc:(tail_call=false) if_let_expr ss loc = fun state ->
    let assigned_val, state = (eval_expr if_let_expr.assigned_expr ss) state in
    if (pattern_matches if_let_expr.pat assigned_val ss loc state) then
        let state = (bind if_let_expr.pat assigned_val ss loc) state in
        (eval_expr ~tc:tail_call if_let_expr.let_then_expr ss) state
    else
        (eval_expr ~tc:tail_call if_let_expr.let_else_expr ss) state

and eval_block_expr ?tc:(tail_call=false) ls ss =
    let static_block_funcs = 
        Preprocess.find_block_funcs ss (ls |> List.map ~f:Located.extract) ss.static_block_funcs 
    in
    let ss = { ss with static_block_funcs } in
    fun state ->
        let (res, _) =
            let len = List.length ls in
            match List.split_n ls (len - 1) with
                | exprs, [last_expr] ->
                    let block_state =
                        List.fold_left 
                            ~init:state 
                            ~f:(fun line_state e -> let _, s = (eval_expr e ss) line_state in s) 
                            exprs
                    in
                    (eval_expr ~tc:tail_call last_expr ss) block_state
                | _ -> assert false (* Unreachable *)
        in (res, state)
    
and eval_match_expr ?tc:(tail_call=false) match_val match_arms ss loc state =
    let (match_val, state) = (eval_expr match_val ss) state in
    let eval_expr expr ?tc:(tc=false) = eval_expr expr ss ~tc:tc in
    let bind lhs rhs = bind lhs rhs ss in
    let result_state_opt = List.find_map ~f:(
        fun (pat, arm_expr, cond) -> 
            if pattern_matches pat match_val ss loc state then
                match cond with
                    | Some cond ->
                        let inner_state = (bind pat match_val loc) state in
                        let cond_eval, inner_state = (eval_expr cond) inner_state in
                        if val_is_true cond_eval ss loc then
                            let (result, _) = (eval_expr ~tc:tail_call arm_expr) inner_state in
                            Some (result, state)
                        else
                            None
                    | None ->
                        let inner_state = (bind pat match_val loc) state in
                        let (result, _) = (eval_expr ~tc:tail_call arm_expr) inner_state in
                        Some(result, state)
            else
                None
    ) match_arms
    in
    match result_state_opt with
        | Some((result, state)) -> 
            result, state
        | None ->
            printf "No patterns matched %s in match expression at %s\n" 
                (string_of_val ss match_val)
                (location_to_string loc);
            print_traceback ss;
            Caml.exit 0

and eval_map_expr ?tc:(_tail_call=false) map_pairs tail_map ss loc state =
    let fold_fn = fun (map_acc, state) (key_expr, val_expr) ->
        let key_val, state = (eval_expr key_expr ss) state in
        let data_val, state = (eval_expr val_expr ss) state in
        let key_hash = hash_value key_val in
        let new_data = match Map.find map_acc key_hash with
            | Some assoc_list -> List.Assoc.add assoc_list ~equal:(fun l r -> val_eq_bool l r ss loc) key_val data_val
            | None -> [(key_val, data_val)]
        in
        (Map.set map_acc ~key:key_hash ~data:new_data, state)
    in
    let tail_map, state = match tail_map with
        | Some e ->
            let m, state = (eval_expr e ss) state in
            Some m, state
        | None -> None, state
    in
    let start_map = match tail_map with
        | Some (Dictionary m) -> m
        | None -> Map.empty (module Int)
        | Some m ->
            printf "Expected a map for the tail in map expr at %s, got %s\n"
                (location_to_string loc)
                (string_of_val ss m);
            print_traceback ss;
            Caml.exit 0
    in
    let (val_map, state) = 
        List.fold_left ~init:(start_map, state) ~f:fold_fn map_pairs
    in (Dictionary val_map, state)

and eval_list_expr ?tc:(_tail_call=false) ls tail ss = fun s ->
    let eval_expr_list ~init =
        List.fold_left
        ~init:init
        ~f:(fun (acc, s) e -> let (ev, s) = (eval_expr e ss) s in (ev::acc, s))
    in
    let eval_prepend ls tail =
        let (tail_eval, s) = (eval_expr tail ss) s in
        match tail_eval with
            | ValList tail_ls ->
                let (eval_ls, state) = eval_expr_list ~init:(tail_ls, s) (List.rev ls) in
                ValList eval_ls, state
            | _ ->
                printf "tried to prepend to non-list %s at %s\n"
                    (string_of_val ss tail_eval)
                    (location_to_string tail.location);
                print_traceback ss;
                Caml.exit 0;
    in
    match tail with
        | Some tail -> eval_prepend ls tail
        | None ->
            let (eval_ls, state) = eval_expr_list ~init:([], s) ls in
            ValList (List.rev eval_ls), state

and eval_expr: (expr Located.t) -> static_state -> ?tc:bool -> state -> value * state = 
    let open Located in
    fun expr ss ?tc:(tail_call=false) -> 
        let eval_prefix_op op e = eval_prefix_op op e ss in
        let eval_op op lhs rhs = eval_op op lhs rhs ss in
        (* printf "Evaluating: %s\n" (string_of_expr expr); *)
        match expr with
        | {data = Atomic v; _} -> fun s -> v, s
        | {data = IdentExpr (UnresolvedIdent name); location} ->
            printf "Error: Found unresolved ident %s at %s\n"
                name
                (location_to_string location);
            print_traceback ss;
            Caml.exit 0
        | {data = IdentExpr (ResolvedIdent i); location} -> eval_ident ss i location
        | {data = Prefix ({op = Head; _} as e); location} -> eval_prefix_op val_list_head e.rhs location
        | {data = Prefix ({op = Tail; _} as e); location} -> eval_prefix_op val_list_tail e.rhs location
        | {data = Prefix ({op = Neg; _} as e); location} -> eval_prefix_op val_negate e.rhs location
        | {data = Prefix ({op = Not; _} as e); location} -> eval_prefix_op val_negate_bool e.rhs location
        | {data = Prefix ({op = _op; _}); location} -> 
            printf "Invalid prefix op at %s\n" (location_to_string location);
            print_traceback ss;
            Caml.exit 0
        | {data = Binary ({op = Add; _} as e); location} -> eval_op val_add e.lhs e.rhs location
        | {data = Binary ({op = Neg; _} as e); location} -> eval_op val_sub e.lhs e.rhs location
        | {data = Binary ({op = Mul; _} as e); location} -> eval_op val_mul e.lhs e.rhs location
        | {data = Binary ({op = Div; _} as e); location} -> eval_op val_div e.lhs e.rhs location
        | {data = Binary ({op = EQ; _} as e); location} -> eval_op val_eq e.lhs e.rhs location
        | {data = Binary ({op = NEQ; _} as e); location} -> eval_op val_neq e.lhs e.rhs location
        | {data = Binary ({op = LEQ; _} as e); location} -> eval_op val_leq e.lhs e.rhs location
        | {data = Binary ({op = GEQ; _} as e); location} -> eval_op val_geq e.lhs e.rhs location
        | {data = Binary ({op = LT; _} as e); location} -> eval_op val_lt e.lhs e.rhs location
        | {data = Binary ({op = GT; _} as e); location} -> eval_op val_gt e.lhs e.rhs location
        | {data = Binary ({op = And; _} as e); location} -> eval_op val_and e.lhs e.rhs location
        | {data = Binary ({op = Or; _} as e); location} -> eval_op val_or e.lhs e.rhs location
        | {data = Binary ({op = Mod; _} as e); location} -> eval_op val_mod e.lhs e.rhs location
        | {data = Binary ({op = PipeOp; _} as e); location} -> eval_pipe ~tc:tail_call e.lhs e.rhs ss location
        | {data = Binary ({op = _op; _}); _} -> assert false (* Invalid binary op *)
        | {data = LambdaDef d; _} -> eval_lambda_def d.lambda_def_expr d.lambda_def_args
        | {data = Let l; location} -> fun s -> (eval_let l.assignee l.assigned_expr ss location) s
        | {data = FnDef d; location} -> fun s -> (eval_fn_def d.fn_name d.fn_def_func ss location) s
        | {data = TupleExpr ls; _} -> fun s -> (eval_tuple_expr ls ss) s
        | {data = LambdaCall l; location} -> fun s -> (eval_lambda_call ~tc:tail_call l ss) location s
        | {data = LambdaCaptureExpr c; location} -> fun s -> (eval_lambda_capture c ss) location s
        | {data = IfExpr i; _} -> eval_if_expr ~tc:tail_call i ss
        | {data = IfLetExpr i; location} -> eval_if_let_expr ~tc:tail_call i ss location
        | {data = BlockExpr ls; _} -> fun s -> (eval_block_expr ~tc:tail_call ls ss) s
        | {data = MatchExpr m; location} -> 
            fun s -> (eval_match_expr ~tc:tail_call m.match_val m.match_arms ss location) s
        | {data = MapExpr (ls, tail); location} -> fun s -> (eval_map_expr ~tc:tail_call ls tail ss location) s
        | {data = ListExpr (ls, tail); _} -> eval_list_expr ls tail ss
        | {data = UnresolvedAtom n; _} ->
            printf "Found unresolved atom :%s\n" n;
            assert false
