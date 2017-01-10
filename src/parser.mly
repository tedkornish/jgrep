%{
module G = Grammar
%}

%token <string> STRINGLIT
%token <string> NUM
%token <Grammar.regex> REGEX
%token GT LT EQUAL MATCHES CONTAINS BEGINSWITH ENDSWITH HASFIELD
%token NOT IS DOES ISNT DOESNT
%token <string> TRUE FALSE

%token AND OR
%right AND OR

%token OPAREN CPAREN

%token EOF

%start predicate
%type <Grammar.predicate> predicate

%%

predicate:
  | m = expr EOF { m };

doesnt:
  | DOESNT { DOESNT }
  | DOES NOT { DOESNT };

isnt:
  | IS NOT { ISNT }
  | ISNT { ISNT };

expr:
  | field filter { G.Pred ($1, $2) }
  | OPAREN expr CPAREN { $2 }
  | expr AND expr { G.And ($1, $3) }
  | HASFIELD field { G.Pred ($2, G.HasField) }
  | doesnt HASFIELD field { G.Pred ($3, G.Not G.HasField) }
  | expr OR expr { G.Or ($1, $3) };

string_lit:
  | STRINGLIT { $1 }
  | NUM { $1 }

does_filter:
  | MATCHES REGEX { G.Matches $2 }
  | MATCHES string_lit { G.Matches (G.Regex $2) }
  | ENDSWITH string_lit { G.EndsWith $2 }
  | BEGINSWITH string_lit { G.BeginsWith $2 }
  | CONTAINS string_lit { G.Contains $2 };

is_filter:
  | IS value { G.Equal $2 }
  | GT NUM { G.GT (float_of_string $2) }
  | LT NUM { G.LT (float_of_string $2) };
      
filter:
  | EQUAL value { G.Equal $2 }
  | does_filter { $1 }
  | IS is_filter { $2 }
  | is_filter { $1 }
  | NOT filter { G.Not $2 }
  | isnt is_filter { G.Not $2 }
  | isnt value { G.Not (G.Equal $2) }
  | doesnt does_filter { G.Not $2 };

value:
  | n = NUM { [G.Num (float_of_string n); G.String n] }
  | s = STRINGLIT { [G.String s] }
  | t = TRUE { [G.String t; G.Bool true] }
  | f = FALSE { [G.String f; G.Bool false] };

field:
  | i = ident { G.Field i };

ident:
  | s = STRINGLIT { s };
