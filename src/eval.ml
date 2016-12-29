open Grammar
open Printf

let format_json_val v = match v with
  | Bool b -> string_of_bool b
  | String s -> sprintf "\"%s\"" s
  | Num f -> sprintf "%f" f

let rec to_jq_predicate_clause (Field f as field) pred =
  let case_insensitive_op field op value =
    sprintf ".%s|ascii_downcase|%s(\"%s\"|ascii_downcase)" field op value in
  match pred with
  | GT n -> sprintf ".%s > %f" f n
  | LT n -> sprintf ".%s < %f" f n
  | Equal v ->
    sprintf "(.%s|ascii_downcase) == (%s|ascii_downcase)" f (format_json_val v)
  | HasField -> sprintf ".|has(\"%s\")" f
  | Not p -> sprintf "%s|not" (to_jq_predicate_clause field p)
  | BeginsWith s -> case_insensitive_op f "startswith" s
  | EndsWith s -> case_insensitive_op f "endswith" s
  | Matches (Regex r) -> sprintf ".%s|test(\"%s\";i)" f r
  | Contains s -> case_insensitive_op f "contains" s

let rec to_jq_predicate pred = match pred with
  | Pred (f, p) -> let clause = to_jq_predicate_clause f p in
    sprintf "(try (%s) catch false)" clause
  | And (e1, e2) ->
    sprintf "(%s and %s)" (to_jq_predicate e1) (to_jq_predicate e2)
  | Or (e1, e2) -> sprintf "((%s) or (%s))" (to_jq_predicate e1) (to_jq_predicate e2)

(* Command, input, and output. *)
type jq_process = JQProcess of string * in_channel * out_channel

let to_jq_selectors selectors =
  let selector_json =
    List.map (fun (Selector s) -> sprintf "\"%s\": .%s" s s) selectors |> String.concat "," in
  let selector_tuple =
    List.map (fun (Selector s) -> sprintf "\"%s\"" s) selectors |> String.concat "," in
  sprintf "{%s} | with_entries(select(.key == (%s))) | del(.[]|nulls)" selector_json selector_tuple

let to_jq (Exp (pred, selectors)) =
  let jq_pred = match pred with None -> "true" | Some p -> to_jq_predicate p in
  let jq_selectors = match selectors with [] -> "." | s -> to_jq_selectors selectors in
  let if_then_stmt = sprintf "if (%s) then (%s) else {} end" jq_pred jq_selectors in
  sprintf "jq --unbuffered -r -c '%s' 2>&1" if_then_stmt

let new_process exp =
  let cmd = to_jq exp in
  let inp, out = Unix.open_process cmd in
  JQProcess (cmd, inp, out)

let close_process (JQProcess (_, inp, out)) =
  let _ = Unix.close_process (inp, out) in ()

let parse_filter s =
  let lexbuf = Lexing.from_string s in
  try (Some (Parser.predicate Lexer.token lexbuf)) with _ -> None

let replace_newlines = Str.global_replace (Str.regexp "\n") "\\\\\\n"

let starts_with s prefix =
  let prefix_length = min (String.length prefix) (String.length s) in
  let s_prefix = Str.first_chars s prefix_length in
  s_prefix = prefix

(* Error message, command that initiated the process, and json input that
   triggered the error. *)
exception Jq_error of string * string * string

let process_line (JQProcess (cmd, inp, out)) (json: string) :string =
  let sanitized = json |> replace_newlines in
  let () = output_string out (sanitized ^ "\n") in
  let () = flush out in 
  let line = input_line inp in
  match starts_with line "jq: error" with
  | true -> raise (Jq_error (line, cmd, json))
  | false -> line
