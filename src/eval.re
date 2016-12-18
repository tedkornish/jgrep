open Grammar;

open Printf;

let formatJsonVal (v: value) :string =>
  switch v {
  | Bool b => string_of_bool b
  | String s => sprintf "\"%s\"" s
  | Num f => sprintf "%f" f
  };

let rec toFilterString (filter: filter) :string =>
  switch filter {
  | GT n => sprintf " > %f" n
  | LT n => sprintf " < %f" n
  | Equal v => sprintf " == %s" (formatJsonVal v)
  | Contains s => sprintf "|contains(\"%s\")" s
  | Matches (Regex r) => sprintf "|test(\"%s\")" r
  | BeginsWith s => sprintf "|startswith(\"%s\")" s
  | EndsWith s => sprintf "|endswith(\"%s\")" s
  | Not f => sprintf "%s|not" (toFilterString f)
  };

let rec toQuery (pred: predicate) :string =>
  switch pred {
  | Pred (Field f) pred => sprintf "(.%s%s)" f (toFilterString pred)
  | And e1 e2 => sprintf "(%s and %s)" (toQuery e1) (toQuery e2)
  | Or e1 e2 => sprintf "(%s or %s)" (toQuery e1) (toQuery e2)
  };

type jqProcess =
  | JQProcess in_channel out_channel;

let newProcess (pred: predicate) :jqProcess => {
  let cmd = toQuery pred |> sprintf "jq --unbuffered -c '%s'" |> String.lowercase;
  let (inp, out) = Unix.open_process cmd;
  JQProcess inp out
};

let closeProcess (JQProcess inp out) => Unix.close_process (inp, out);

let parseFilter s :option predicate => {
  let lexbuf = Lexing.from_string s;
  try (Some (Parser.predicate Lexer.token lexbuf)) {
  | _ => None
  }
};

let replaceNewlines = Str.global_replace (Str.regexp "\n") "\\\\\\n";

let passesFilter (JQProcess inp out) (json: string) :bool => {
  let lowerJson = String.lowercase json |> replaceNewlines;
  let () = output_string out (lowerJson ^ "\n");
  let () = flush out;
  let line = input_line inp;
  line |> bool_of_string
};
