open Utils
open Grammar
open OUnit2

let passes_filter predicate json =
  let output = exp_output (Exp ((Some predicate), [])) json in output <> "{}"

let positive_cases = [
  ("predicate passes with simple object", (Pred ((Field "age"), (GT 30.0))), "{\"age\":37}");
  ("'and' passes when both clauses pass", (And (Pred (Field "age", GT 30.0), (Pred (Field "state", BeginsWith "c")))), "{\"age\":37,\"state\":\"CA\"}");
  ("'or' passes when one clause passes", (Or (Pred (Field "age", GT 30.0), (Pred (Field "state", BeginsWith "c")))), "{\"age\":37,\"state\":\"MI\"}");
  ("negated predicate passes with simple object", (Pred ((Field "age"), (Not (GT 30.0)))), "{\"age\":27}");
  ("works with an escaped newline in the json doc", (Pred ((Field "age"), (Not (GT 30.0)))), "{\"age\":27,\"hello\":\"wor\nld\"}");
  ("passes the 'has field' filter with field and null value", (Pred ((Field "age"), HasField)), "{\"age\": null}");
  ("string equality is case-insensitive", (Pred (Field "name", Equal [(String "ygritte")])), "{\"name\": \"Ygritte\"}");
  ("basic contains filter works", (Pred (Field "name", Contains "snow")), "{\"name\": \"Jon Snow\"}");
  ("simple equality for bools works", (Pred (Field "cool", Equal [(Bool true)])), "{\"cool\": true}");
  ("simple equality for numbers works", (Pred (Field "number", Equal [(Num 9.00)])), "{\"number\": 9}");
  ("endsWith works", (Pred (Field "name", EndsWith "now")), "{\"name\": \"Jon Snow\"}");
  ("less than works", (Pred (Field "number", LT (-9.00))), "{\"number\": -9.02}");
  ("matches works", (Pred (Field "world", Matches (Regex "h.LM{0}\"?;*lo$"))), "{\"world\": \"hello\"}");
  (
    "equality with multiple senses matches the bool value true",
    (Pred (Field "world", Equal [(String "t"); (Bool true)])),
    "{\"world\": true}"
  );
  (
    "equality with multiple senses matches the string value true",
    (Pred (Field "world", Equal [(String "t"); (Bool true)])),
    "{\"world\": \"t\"}"
  );
  (
    "equality with multiple senses matches the bool value true",
    (Pred (Field "world", Equal [(String "f"); (Bool false)])),
    "{\"world\": false}"
  );
]

let negative_cases = [
  ("predicate fails with empty object", (Pred ((Field "age"), (GT 30.0))), "{}");
  ("'and' fails when one clause fails", (And (Pred (Field "age", GT 30.0), (Pred (Field "state", BeginsWith "c")))), "{\"age\":37,\"state\":\"MI\"}");
  ("doesn't pass the 'has field' filter without field", (Pred ((Field "age"), HasField)), "{\"name\": \"Jon Snow\"}");
  ("contains doesn't throw an exception for non-string json values", (Pred (Field "name", Contains "snow")), "{\"name\": null}");
]

let suite =
  "predicate integration test suite" >::: List.append
    (List.map (fun (reason, pred, json_string) ->
         reason >:: (fun ctxt -> assert_bool reason (passes_filter pred json_string))
       ) positive_cases)
    (List.map (fun (reason, pred, json_string) ->
         reason >:: (fun ctxt -> assert_bool reason (passes_filter pred json_string |> not))
       ) negative_cases)
