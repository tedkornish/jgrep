open Utils
open Grammar
open OUnit2

let passes_filter predicate json =
  let output = exp_output (Exp ((Some predicate), [])) json in output <> "{}"
let test1 ctx = assert_bool "test1"
    (passes_filter (Pred ((Field "age"), (GT 30.0))) "{}" |> not)
let test2 ctx = assert_bool "test2"
    (passes_filter (Pred ((Field "age"), (GT 30.0))) "{\"age\":37}")
let test3 ctx = assert_bool "test3"
    (passes_filter
       (And (Pred (Field "age", GT 30.0), (Pred (Field "state", BeginsWith "c"))))
       "{\"age\":37,\"state\":\"CA\"}")
let test4 ctx =
  let e = And (Pred (Field "age", GT 30.0), (Pred (Field "state", BeginsWith "c"))) in
  let j = "{\"age\":37,\"state\":\"MI\"}" in
  assert_bool "test4" (passes_filter e j |> not)
let test5 ctx = assert_bool "test5"
    (passes_filter
       (Or (Pred (Field "age", GT 30.0), (Pred (Field "state", BeginsWith "c"))))
       "{\"age\":37,\"state\":\"MI\"}")
let test6 ctx = assert_bool "test6"
    (passes_filter
       (Pred ((Field "age"), (Not (GT 30.0))))
       "{\"age\":27}")
let test7 ctx = assert_bool "test7"
    (passes_filter
       (Pred ((Field "age"), (Not (GT 30.0))))
       "{\"age\":27,\"hello\":\"wor\nld\"}")
let suite = "predicate integration test suite" >::: [
    "predicate fails with empty object" >:: test1;
    "predicate passes with simple object" >:: test2;
    "and passes when both clauses pass" >:: test3;
    "and fails when one clause fails" >:: test4;
    "or passes when one clause passes" >:: test5;
    "negated predicate passes with simple object" >:: test6;
    "works with an escaped newline in the json doc" >:: test7
  ]
