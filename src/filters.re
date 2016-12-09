open Grammar;

let parseFilter s :option Grammar.exp => {
  let lexbuf = Lexing.from_string s;
  try (Some (Parser.filterExp Lexer.token lexbuf)) {
  | _ => None
  }
};
