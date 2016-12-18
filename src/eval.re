open Grammar;

open Printf;

let formatJsonVal (v: value) :string =>
  switch v {
  | Bool b => string_of_bool b
  | String s => sprintf "\"%s\"" s
  | Num f => sprintf "%f" f
  };

let rec toJqFilter (filter: filter) :string =>
  switch filter {
  | GT n => sprintf " > %f" n
  | LT n => sprintf " < %f" n
  | Equal v => sprintf " == %s" (formatJsonVal v)
  | Contains s => sprintf "|contains(\"%s\")" s
  | Matches (Regex r) => sprintf "|test(\"%s\")" r
  | BeginsWith s => sprintf "|startswith(\"%s\")" s
  | EndsWith s => sprintf "|endswith(\"%s\")" s
  | Not f => sprintf "%s|not" (toJqFilter f)
  };

let rec toJqPredicate (pred: predicate) :string =>
  switch pred {
  | Pred (Field f) pred => sprintf "(.%s%s)" f (toJqFilter pred)
  | And e1 e2 => sprintf "(%s and %s)" (toJqPredicate e1) (toJqPredicate e2)
  | Or e1 e2 => sprintf "(%s or %s)" (toJqPredicate e1) (toJqPredicate e2)
  };

type jqProcess =
  | JQProcess in_channel out_channel;

let toJqSelectors selectors => raise Not_found;

let newProcess (Exp pred selectors) :jqProcess => {
  let jqPredicate =
    switch pred {
    | None => "true"
    | Some p => toJqPredicate p
    };
  let jqSelectors =
    switch selectors {
    | [] => "."
    | s => toJqSelectors selectors
    };
  let ifThen = sprintf "if (%s) then (%s) else \"{}\" end" jqPredicate jqSelectors;
  let cmd = sprintf "jq --unbuffered -r -c '%s'" ifThen |> String.lowercase;
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

let processLine (JQProcess inp out) (json: string) :string => {
  let lowerJson = String.lowercase json |> replaceNewlines;
  let () = output_string out (lowerJson ^ "\n");
  let () = flush out;
  input_line inp
};
