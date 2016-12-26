open OUnit2
open Eval
open Grammar

let exp_output exp json =
  let (JQProcess (inp, out)) = new_process exp in
  let lower_json = String.lowercase json |> replace_newlines in
  let () = output_string out (lower_json ^ "\n") in
  let () = flush out in
  let line = input_line inp in
  let _ = close_process (JQProcess (inp, out)) in
  line

module Exp =
  struct
    let test1 ctx = assert_equal (exp_output (Exp (None, [])) "{}") "{}"
    let test2 ctx = assert_equal
      "{\"hello\":\"world\"}"
      (exp_output
        (Exp (None, [Selector "hello"]))
        "{\"x\": \"y\", \"hello\": \"world\"}")
    let suite = "predicate integration exp suite" >::: [
      "with empty object and no predicate and no selectors" >:: test1;
      "with selectors as a subset of object keys" >:: test2
    ]
  end

module Predicate =
  struct
    let suite = "predicate integration test suite" >::: []
  end
