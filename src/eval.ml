open Grammar
open Printf

let case_insensitive_string_op field op value =
  sprintf ".%s|ascii_downcase|%s(\"%s\"|ascii_downcase)" field op value

let case_insensitive_array_op field op value =
  sprintf ".%s|map(try_downcase(.))|%s([%s|try_downcase(.)])" field op value

let runtime = "
  def try_downcase($s): try ascii_downcase catch $s
"

let to_jq_equality_clause (Field f) v =
  let clause = (match v with
  | String s ->
    sprintf "(.%s|ascii_downcase) == (\"%s\"|ascii_downcase)" f s
  | Bool true -> sprintf "(.%s)" f
  | Bool false -> sprintf "(.%s|not)" f
  | Num n -> sprintf "(.%s) == (%f)" f n) in
  sprintf "(try (%s) catch false)" clause

let to_jq_contains_clauses (Field f) v :string list =
  let clauses = (match v with
      | String s -> [ (* for strings, do both substring and array inclusion *)
          case_insensitive_string_op f "contains" s;
          case_insensitive_array_op f "contains" (sprintf "\"%s\"" s);
        ]
      | Bool b -> [ (* for bools and numbers, check only for array inclusion *)
          case_insensitive_array_op f "contains" (string_of_bool b);
        ]
      | Num n -> [
          case_insensitive_array_op f "contains" (string_of_float n);
        ]
    ) in
  let wrap_in_try_catch = sprintf ("(try (%s) catch false)") in
  List.map wrap_in_try_catch clauses

let rec to_jq_predicate_clause (Field f as field) pred =
  let pred_string = match pred with
  | GT n -> sprintf ".%s > %f" f n
  | LT n -> sprintf ".%s < %f" f n
  | Equal vs -> List.map (to_jq_equality_clause field) vs |>
                String.concat " or " |>
                sprintf "(%s)"
  | HasField -> sprintf ".|has(\"%s\")" f
  | Not p -> sprintf "%s|not" (to_jq_predicate_clause field p)
  | BeginsWith s -> case_insensitive_string_op f "startswith" s
  | EndsWith s -> case_insensitive_string_op f "endswith" s
  | Matches (Regex r) -> sprintf ".%s|test(\"%s\"; \"i\")" f (String.escaped r)
  | Contains vs -> List.map (to_jq_contains_clauses field) vs |>
                   List.flatten |>
                   String.concat " or " |>
                   sprintf "(%s)" in
  sprintf "(try (%s) catch false)" pred_string

(* Turn a field and tree of predicates into nested jq filter expressions. It
   wraps each single predicate into a try/catch block so that, if a type error
   occurs and throws an exception in the jq runtime (e.g.trying to see if a
   number begins with a string), that single predicate defaults to [false] and
   the program keeps executing. *)
let rec to_jq_predicate pred = match pred with
  | Pred (f, p) -> to_jq_predicate_clause f p
  | And (e1, e2) ->
    sprintf "(%s and %s)" (to_jq_predicate e1) (to_jq_predicate e2)
  | Or (e1, e2) -> sprintf "(%s or %s)" (to_jq_predicate e1) (to_jq_predicate e2)

type jq_process = JQProcess of string * in_channel * out_channel

(* Transform a list of selectors into a jq program which will extract non-null
   values from those keys, in order, from a given json object. *)
let to_jq_json_selectors selectors =
  let selector_json =
    List.map (fun (Selector s) -> sprintf "\"%s\": .%s" s s) selectors |>
    String.concat "," in
  let selector_tuple =
    List.map (fun (Selector s) -> sprintf "\"%s\"" s) selectors |>
    String.concat "," in
  match selectors with
  | [] -> "."
  | _ -> sprintf "{%s} | with_entries(select(.key == (%s))) | del(.[]|nulls)"
           selector_json
           selector_tuple

(* Transform a list of selectors into a jq program which will extract values,
   null or otherwise, from a given JSON document in order as a TSV. *)
let to_jq_tsv_selectors selectors =
  let selector_names =
    List.map (fun (Selector s) -> sprintf ".\"%s\"" s) selectors |>
    String.concat "," in
  sprintf "[%s] | @tsv" selector_names

let to_jq (Exp (pred, selectors)) ~tsv =
  let jq_pred = match pred with None -> "true" | Some p -> to_jq_predicate p in
  let jq_selectors = (match tsv with
    | true -> to_jq_tsv_selectors selectors
    | false -> to_jq_json_selectors selectors) in
  let if_then_stmt = sprintf "%s; if (%s) then (%s) else {} end"
      runtime
      jq_pred
      jq_selectors in
  sprintf "jq --unbuffered -r -c '%s' 2>&1" if_then_stmt

let new_process ?(tsv = false) exp =
  let cmd = to_jq exp ~tsv:tsv in
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

exception Jq_error of string * string * string

let process_line (JQProcess (cmd, inp, out)) (json: string) :string =
  let sanitized = json |> replace_newlines in
  let () = output_string out (sanitized ^ "\n") in
  let () = flush out in 
  let line = input_line inp in
  match starts_with line "jq: error" with
  | true -> raise (Jq_error (line, cmd, json))
  | false -> line
