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

let toFilterString (filter: filter) :string =>
  switch filter {
  | GT n => sprintf "> %f" n
  };

let toQuery (filter: exp) :string =>
  switch filter {
  | Exp (Field f) filter => sprintf ".%s %s" f (toFilterString filter)
  };

let passesFilter (filter: exp) (json: string) :bool => {
  let jqQuery = toQuery filter;
  let jqCommand = sprintf "echo '%s' | jq '%s'" json jqQuery;
  let result = run jqCommand;
  bool_of_string result
};
