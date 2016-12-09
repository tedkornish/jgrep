open OUnit2;

open Filters;

open Grammar;

let test1 ctx => assert_equal (passesFilter (Exp (Field "age") (GT 30.0)) "{}") false;

let test2 ctx => assert_equal (passesFilter (Exp (Field "age") (GT 30.0)) "{\"age\":37}") true;

let suite = "integration test suite" >::: ["test1" >:: test1, "test2" >:: test2];
