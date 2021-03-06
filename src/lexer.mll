{
open Parser
open Grammar
}

let str = ['a'-'z' 'A'-'Z' '_' '"' '*' '?' '0'-'9' '^' '$' '|' '.' '\'' '-']+
let strWithSpecialChars = (str | ' ' | '(' | ')' | '-')+
let digit = ['0'-'9']
let int = digit+
let num = '-'? ['0'-'9']* ('.' ['0'-'9']+)?
let ident = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule token = parse
  | '(' { OPAREN }
  | ')' { CPAREN }
  | [' ' '\t']+ { token lexbuf } (* tail-recurse to the next token *)
  | "is" { IS }
  | '=' { EQUAL }
  | '>' | "greater than" { GT }
  | '<' | "less than" { LT }
  | "and" | "&&" { AND }
  | "or" | "||" { OR }
  | ("true" | "t") as s { TRUE s }
  | ("false" | "f") as s { FALSE s }
  | "matches" { MATCHES }
  | "ends with" { ENDSWITH }
  | ("start" | "begin") "s"? " with" { BEGINSWITH }
  | "contain" "s"? { CONTAINS }
  | "isn't" { ISNT }
  | "does" { DOES }
  | "doesn't" { DOESNT }
  | "not" { NOT }
  | ("has" | "have") " " ("field" | "key") { HASFIELD }
  | '/' (strWithSpecialChars as s) '/' { REGEX (Regex s) }
  | num as n { NUM n }
  | '"' (strWithSpecialChars as s) '"' { STRINGLIT s }
  | "'" (strWithSpecialChars as s) "'" {STRINGLIT s }
  | str as s { STRINGLIT s }
  | ident as i { STRINGLIT i }
  | eof { EOF }
