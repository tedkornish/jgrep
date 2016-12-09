open OUnit2;

open Filters;

open Grammar;

let test1 ctx => assert_equal (passesFilter (Exp (Field "age") (GT 30.0)) "{}") false;

let test2 ctx => assert_equal (passesFilter (Exp (Field "age") (GT 30.0)) "{\"age\":37}") true;

let test3 ctx =>
  assert_equal
    (
      passesFilter
        (And (Exp (Field "age") (GT 30.0)) (Exp (Field "state") (BeginsWith "c")))
        "{\"age\":37,\"state\":\"CA\"}"
    )
    true;

let test4 ctx =>
  assert_equal
    (
      passesFilter
        (And (Exp (Field "age") (GT 30.0)) (Exp (Field "state") (BeginsWith "c")))
        "{\"age\":37,\"state\":\"MI\"}"
    )
    false;

let test5 ctx =>
  assert_equal
    (
      passesFilter
        (Or (Exp (Field "age") (GT 30.0)) (Exp (Field "state") (BeginsWith "c")))
        "{\"age\":37,\"state\":\"MI\"}"
    )
    true;

let test6 ctx =>
  assert_equal (passesFilter (Exp (Field "age") (Not (GT 30.0))) "{\"age\":27}") true;

let suite =
  "integration test suite" >::: [
    "predicate fails with empty object" >:: test1,
    "predicate passes with simple object" >:: test2,
    "and passes when both clauses pass" >:: test3,
    "and fails when one clause fails" >:: test4,
    "or passes when one clause passes" >:: test5,
    "negated predicate passes with simple object" >:: test6
  ];
