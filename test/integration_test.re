open OUnit2;

open Eval;

open Grammar;

let expOutput (exp: exp) (json: string) :string => {
  let JQProcess inp out = newProcess exp;
  let lowerJson = String.lowercase json |> replaceNewlines;
  let () = output_string out (lowerJson ^ "\n");
  let () = flush out;
  let line = input_line inp;
  let _ = closeProcess (JQProcess inp out);
  line
};

let module Exp = {
  let test1 ctx => assert_equal (expOutput (Exp None []) "{}") "{}";
  let test2 ctx =>
    assert_equal
      printer::[%show : string]
      "{\"hello\":\"world\"}"
      (expOutput (Exp None [Selector "hello"]) "{\"x\": \"y\", \"hello\": \"world\"}");
  let test3 ctx =>
    assert_equal
      printer::[%show : string]
      "{\"hello\":\"world\"}"
      (
        expOutput
          (Exp None [Selector "hello", Selector "merp"]) "{\"x\": \"y\", \"hello\": \"world\"}"
      );
  let test4 ctx =>
    assert_equal
      printer::[%show : string]
      "{\"hello\":\"world\",\"x\":\"y\",\"a\":8}"
      (
        expOutput
          (Exp None [Selector "hello", Selector "x", Selector "a"])
          "{\"a\": 8, \"x\": \"y\", \"hello\": \"world\"}"
      );
  let suite =
    "predicate integration exp suite" >::: [
      "with empty object and no predicate and no selectors" >:: test1,
      "with selectors as a subset of object keys" >:: test2,
      "with selector on a key not in the object" >:: test3,
      "with selector in an order different from the object's keys" >:: test4
    ];
};

let module Predicate = {
  let passesFilter (predicate: predicate) (json: string) :bool =>
    expOutput (Exp (Some predicate) []) json != "{}";
  let test1 ctx => assert_equal (passesFilter (Pred (Field "age") (GT 30.0)) "{}") false;
  let test2 ctx => assert_equal (passesFilter (Pred (Field "age") (GT 30.0)) "{\"age\":37}") true;
  let test3 ctx =>
    assert_equal
      (
        passesFilter
          (And (Pred (Field "age") (GT 30.0)) (Pred (Field "state") (BeginsWith "c")))
          "{\"age\":37,\"state\":\"CA\"}"
      )
      true;
  let test4 ctx =>
    assert_equal
      (
        passesFilter
          (And (Pred (Field "age") (GT 30.0)) (Pred (Field "state") (BeginsWith "c")))
          "{\"age\":37,\"state\":\"MI\"}"
      )
      false;
  let test5 ctx =>
    assert_equal
      (
        passesFilter
          (Or (Pred (Field "age") (GT 30.0)) (Pred (Field "state") (BeginsWith "c")))
          "{\"age\":37,\"state\":\"MI\"}"
      )
      true;
  let test6 ctx =>
    assert_equal (passesFilter (Pred (Field "age") (Not (GT 30.0))) "{\"age\":27}") true;
  let test7 ctx =>
    assert_equal
      (passesFilter (Pred (Field "age") (Not (GT 30.0))) "{\"age\":27,\"hello\":\"wor\nld\"}")
      true;
  let suite =
    "predicate integration test suite" >::: [
      "predicate fails with empty object" >:: test1,
      "predicate passes with simple object" >:: test2,
      "and passes when both clauses pass" >:: test3,
      "and fails when one clause fails" >:: test4,
      "or passes when one clause passes" >:: test5,
      "negated predicate passes with simple object" >:: test6,
      "works with an escaped newline in the json doc" >:: test7
    ];
};
