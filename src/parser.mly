%{
module G = Grammar
%}

%token <string> STRINGLIT
%token <float> NUM
%token <Grammar.regex> REGEX
%token GT LT EQUAL MATCHES CONTAINS BEGINSWITH ENDSWITH HASFIELD
%token NOT IS DOES ISNT DOESNT

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

does_filter:
  | MATCHES REGEX { G.Matches $2 }
  | ENDSWITH STRINGLIT { G.EndsWith $2 }
  | BEGINSWITH STRINGLIT { G.BeginsWith $2 }
  | CONTAINS STRINGLIT { G.Contains $2 };

is_filter:
  | IS value { G.Equal $2 }
  | GT NUM { G.GT $2 }
  | LT NUM { G.LT $2 };
      
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
  | n = NUM { G.Num n }
  | s = STRINGLIT { G.String s };

field:
  | i = ident { G.Field i };

ident:
  | s = STRINGLIT { s };
