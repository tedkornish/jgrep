open OUnit2
open Eval
open Grammar

let exp_output exp json =
  let p = new_process exp in
  let line = process_line p json in
  let _ = close_process p in
  line

module Exp = struct
  let test1 ctx = assert_equal (exp_output (Exp (None, [])) "{}") "{}"
  let test2 ctx = assert_equal
      "{\"hello\":\"world\"}"
      (exp_output
         (Exp (None, [Selector "hello"]))
         "{\"x\": \"y\", \"hello\": \"world\"}")
  let test3 ctx =
    assert_equal
      "{\"hello\":\"world\"}"
      (
        exp_output
          (Exp (None, [Selector "hello"; Selector "merp"])) "{\"x\": \"y\", \"hello\": \"world\"}"
      )
  let test4 ctx =
    assert_equal
      "{\"hello\":\"world\",\"x\":\"y\",\"a\":8}"
      (
        exp_output
          (Exp (None, [Selector "hello"; Selector "x"; Selector "a"]))
          "{\"a\": 8, \"x\": \"y\", \"hello\": \"world\"}"
      )
  let test5 ctx = assert_equal
      ~printer:(fun x -> x)
      "{\"myKey\":\"myVal\"}"
      (exp_output (Exp (None, [Selector "myKey"])) "{\"myKey\": \"myVal\", \"Merp\": 19}")
  let suite = "exp integration test suite" >::: [
      "with empty object and no predicate and no selectors" >:: test1;
      "with selectors as a subset of object keys" >:: test2;
      "with selector on a key not in the object" >:: test3;
      "with selector in an order different from the object's keys" >:: test4;
      "with selector with capital letters and values" >:: test5
    ]
end

module Predicate = struct
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
end
