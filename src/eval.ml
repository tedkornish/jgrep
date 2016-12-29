open Grammar
open Printf

let format_json_val v = match v with
  | Bool b -> string_of_bool b
  | String s -> sprintf "\"%s\"" s
  | Num f -> sprintf "%f" f

let rec to_jq_filter filter = match filter with
  | GT n -> sprintf " > %f" n
  | LT n -> sprintf " < %f" n
  | Equal v -> sprintf " == %s" (format_json_val v)
  | Contains s -> sprintf "|contains(\"%s\")" s
  | Matches (Regex r) -> sprintf "|test(\"%s\")" r
  | BeginsWith s -> sprintf "|startswith(\"%s\")" s
  | EndsWith s -> sprintf "|endswith(\"%s\")" s
  | Not f -> sprintf "%s|not" (to_jq_filter f)

let rec to_jq_predicate pred = match pred with
  | Pred ((Field f), p) -> sprintf "(.%s%s)" f (to_jq_filter p)
  | And (e1, e2) -> sprintf "(%s and %s)" (to_jq_predicate e1) (to_jq_predicate e2)
  | Or (e1, e2) -> sprintf "(%s or %s)" (to_jq_predicate e1) (to_jq_predicate e2)

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
  sprintf "jq --unbuffered -r -c '%s'" if_then_stmt |> String.lowercase

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

let process_line (JQProcess (inp, out)) (json: string) :string =
  let lower_json = String.lowercase json |> replace_newlines in
  let () = output_string out (lower_json ^ "\n") in
  let () = flush out in 
  input_line inp
