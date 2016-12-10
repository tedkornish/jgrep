open Grammar;

open Printf;

let parseFilter s :option Grammar.exp => {
  let lexbuf = Lexing.from_string s;
  try (Some (Parser.filterExp Lexer.token lexbuf)) {
  | _ => None
  }
};

let run (cmd: string) :string => {
  let inp = Unix.open_process_in cmd;
  let r = input_line inp;
  close_in inp;
  r
};

let formatJsonVal (v: value) :string => raise Not_found;

let rec toFilterString (filter: filter) :string =>
  switch filter {
  | GT n => sprintf "> %f" n
  | LT n => sprintf "< %f" n
  | Equal v => sprintf "== %s" (formatJsonVal v)
  | Contains s => sprintf "|contains(\"%s\")" s
  | Matches (Regex r) => sprintf "|test(\"%s\")" r
  | BeginsWith s => sprintf "|startswith(\"%s\")" s
  | EndsWith s => sprintf "|endswith(\"%s\")" s
  | Not f => sprintf "%s|not" (toFilterString f)
  };

let rec toQuery (filter: exp) :string =>
  switch filter {
  | Exp (Field f) filter => sprintf "(.%s %s)" f (toFilterString filter)
  | And e1 e2 => sprintf "(%s and %s)" (toQuery e1) (toQuery e2)
  | Or e1 e2 => sprintf "(%s or %s)" (toQuery e1) (toQuery e2)
  };

let passesFilter (filter: exp) (json: string) :bool => {
  let jqQuery = toQuery filter;
  let lowerJson = String.lowercase json;
  let jqCommand = sprintf "echo '%s' | jq '%s'" lowerJson jqQuery;
  run jqCommand |> bool_of_string
};
