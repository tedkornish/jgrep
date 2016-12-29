open OUnit2
open Grammar
open Utils

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
let suite = "selector integration test suite" >::: [
    "with empty object and no predicate and no selectors" >:: test1;
    "with selectors as a subset of object keys" >:: test2;
    "with selector on a key not in the object" >:: test3;
    "with selector in an order different from the object's keys" >:: test4;
    "with selector with capital letters and values" >:: test5
  ]
