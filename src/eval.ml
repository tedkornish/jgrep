open Grammar
open Printf

let format_json_val v = match v with
  | Bool b -> string_of_bool b
  | String s -> sprintf "\"%s\"" s
  | Num f -> sprintf "%f" f

let rec to_jq_predicate pred = match pred with
  | Pred (Field f, GT n) -> sprintf ".%s > %f" f n
  | Pred (Field f, LT n) -> sprintf ".%s < %f" f n
  | Pred (Field f, Equal v) ->
    sprintf "(.%s|ascii_downcase) == (%s|ascii_downcase)" f (format_json_val v)
  | Pred (Field f, HasField) -> sprintf ".|has(\"%s\")" f
  | Pred (Field f, Not p) ->
    sprintf "%s|not" (to_jq_predicate (Pred (Field f, p)))
  | Pred (Field f, BeginsWith s) ->
    sprintf ".%s|ascii_downcase|startswith(\"%s\"|ascii_downcase)" f s
  | Pred (Field f, EndsWith s) ->
    sprintf ".%s|ascii_downcase|endswith(\"%s\"|ascii_downcase)" f s
  | Pred (Field f, Matches (Regex r)) -> sprintf ".%s|test(\"%s\";i)" f r
  | Pred (Field f, Contains s) ->
    sprintf "%.s|ascii_downcase|contains(\"%s\"|ascii_downcase)" f s
  | And (e1, e2) ->
    sprintf "((%s) and (%s))" (to_jq_predicate e1) (to_jq_predicate e2)
  | Or (e1, e2) -> sprintf "((%s) or (%s))" (to_jq_predicate e1) (to_jq_predicate e2)

type jq_process = JQProcess of in_channel * out_channel

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
  JQProcess (inp, out)

let close_process (JQProcess (inp, out)) =
  let _ = Unix.close_process (inp, out) in ()

let parse_filter s =
  let lexbuf = Lexing.from_string s in
  try (Some (Parser.predicate Lexer.token lexbuf)) with _ -> None

let replace_newlines = Str.global_replace (Str.regexp "\n") "\\\\\\n"

let starts_with s prefix =
  let prefix_length = min (String.length prefix) (String.length s) in
  let s_prefix = Str.first_chars s prefix_length in
  s_prefix = prefix

exception Jq_error of string

let process_line (JQProcess (inp, out)) (json: string) :string =
  let sanitized = json |> replace_newlines in
  let () = output_string out (sanitized ^ "\n") in
  let () = flush out in 
  let line = input_line inp in
  match starts_with line "jq: error" with
  | true -> raise (Jq_error line)
  | false -> line
