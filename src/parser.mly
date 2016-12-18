%{
module G = Grammar
%}

%token <string> STRINGLIT
%token <float> NUM
%token <Grammar.regex> REGEX
%token GT LT EQUAL MATCHES CONTAINS BEGINSWITH ENDSWITH

%token AND OR
%right AND OR

%token OPAREN CPAREN

%token EOF

%start predicate
%type <Grammar.predicate> predicate

%%

predicate:
  | m = expr EOF { m };

expr:
  | field filter { G.Pred ($1, $2) }
  | OPAREN expr CPAREN { $2 }
  | expr AND expr { G.And ($1, $3) }
  | expr OR expr { G.Or ($1, $3) };

filter:
  | EQUAL value { G.Equal $2 }
  | GT NUM { G.GT $2 }
  | LT NUM { G.LT $2 }
  | MATCHES REGEX { G.Matches $2 };
  | ENDSWITH STRINGLIT { G.EndsWith $2 };
  | BEGINSWITH STRINGLIT { G.BeginsWith $2 };
  | CONTAINS STRINGLIT { G.Contains $2 };

value:
  | n = NUM { G.Num n }
  | s = STRINGLIT { G.String s }

field:
  | i = ident { G.Field i };

ident:
  | s = STRINGLIT { s };
