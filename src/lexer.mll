{
open Parser
open Grammar
module G = Grammar
}

let str = ['a'-'z' 'A'-'Z' '_' '"' '*' '?' '0'-'9' '^' '$' '|' '.' '\'']+
let strWithSpecialChars = (str | ' ' | '(' | ')' | '-')+
let digit = ['0'-'9']
let int = digit+
let num = '-'? ['0'-'9']* ('.' ['0'-'9']+)?
let ident = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule token = parse
  | '(' { OPAREN }
  | ')' { CPAREN }
  | [' ' '\t']+ { token lexbuf }
  | "is" { IS }
  | '=' { EQUAL }
  | '>' | "greater than" { GT }
  | '<' | "less than" { LT }
  | "and" { AND }
  | "or" { OR }
  | "matches" { MATCHES }
  | "ends with" { ENDSWITH }
  | ("starts" | "begins") " with" { BEGINSWITH }
  | "contains" { CONTAINS }
  | "isn't" { ISNT }
  | "does" { DOES }
  | "doesn't" { DOESNT }
  | "not" { NOT }
  | ("has" | "have") " " ("field" | "key") { HASFIELD }
  | '/' (strWithSpecialChars as s) '/' { REGEX (G.Regex s) }
  | num { NUM (float_of_string (Lexing.lexeme lexbuf)) }
  | '"' (strWithSpecialChars as s) '"' { STRINGLIT s }
  | "'" (strWithSpecialChars as s) "'" {STRINGLIT s }
  | str { STRINGLIT (Lexing.lexeme lexbuf) }
  | ident { STRINGLIT (Lexing.lexeme lexbuf) }
  | eof { EOF }
