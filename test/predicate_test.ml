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
let test8 ctx = assert_bool "test8"
    (passes_filter
       (Pred ((Field "age"), HasField))
       "{\"age\": null}")
let test9 ctx = assert_bool "test9"
    (passes_filter
       (Pred ((Field "age"), HasField))
       "{\"name\": \"Jon Snow\"}" |> not)
let test10 ctx = assert_bool "test10"
    (passes_filter
       (Pred (Field "name", Equal [(String "ygritte")]))
       "{\"name\": \"Ygritte\"}")
let test11 ctx = assert_bool "test11"
    (passes_filter
       (Pred (Field "name", Contains "snow"))
       "{\"name\": \"Jon Snow\"}")
let test12 ctx = assert_bool "test12"
    (passes_filter
       (Pred (Field "name", Contains "snow"))
       "{\"name\": null}" |> not)
let test13 ctx = assert_bool "test13"
    (passes_filter
       (Pred (Field "cool", Equal [(Bool true)]))
       "{\"cool\": true}")
let test14 ctx = assert_bool "test14"
    (passes_filter
       (Pred (Field "number", Equal [(Num 9.00)]))
       "{\"number\": 9}")
let test15 ctx = assert_bool "test15"
    (passes_filter
       (Pred (Field "name", EndsWith "now"))
       "{\"name\": \"Jon Snow\"}")
let test16 ctx = assert_bool "test16"
    (passes_filter
       (Pred (Field "number", LT (-9.00)))
       "{\"number\": -9.02}")
let test17 ctx = assert_bool "test17"
    (passes_filter
       (Pred (Field "world", Matches (Regex "h.LM{0}\"?;*lo$")))
       "{\"world\": \"hello\"}")
let test18 ctx = assert_bool "test18"
    (passes_filter
       (Pred (Field "world", Equal [(String "t"); (Bool true)]))
       "{\"world\": true}")
let test19 ctx = assert_bool "test19"
    (passes_filter
       (Pred (Field "world", Equal [(String "t"); (Bool true)]))
       "{\"world\": \"t\"}")
let test20 ctx = assert_bool "test20"
    (passes_filter
       (Pred (Field "world", Equal [(String "f"); (Bool false)]))
       "{\"world\": false}")
let suite = "predicate integration test suite" >::: [
    "predicate fails with empty object" >:: test1;
    "predicate passes with simple object" >:: test2;
    "and passes when both clauses pass" >:: test3;
    "and fails when one clause fails" >:: test4;
    "or passes when one clause passes" >:: test5;
    "negated predicate passes with simple object" >:: test6;
    "works with an escaped newline in the json doc" >:: test7;
    "passes the 'has field' filter with field" >:: test8;
    "doesn't pass the 'has field' filter without field" >:: test9;
    "string equality is case-insensitive" >:: test10;
    "basic contains filter works" >:: test11;
    "contains doesn't throw an exception for non-string json values" >:: test12;
    "simple equality for bools works" >:: test13;
    "simple equality for numbers works" >:: test14;
    "endsWith works" >:: test15;
    "less than works" >:: test16;
    "matches works" >:: test17;
    "equality with multiple senses matches the bool value true" >:: test18;
    "equality with multiple senses matches the string value true" >:: test19;
    "equality with multiple senses matches the bool value true" >:: test20;
  ]
