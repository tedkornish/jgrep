open OUnit2
open Grammar
open Utils

let cases = [
  ("with empty object and no selectors", "{}", [], "{}");
  ("with selectors as a subset of object keys", "{\"hello\":\"world\"}", ["hello"], "{\"x\": \"y\", \"hello\": \"world\"}");
  ("with selector on a key not in the object", "{\"hello\":\"world\"}", ["hello"; "merp"], "{\"x\": \"y\", \"hello\": \"world\"}");
  ("with selectors in an order different from the object's keys", "{\"hello\":\"world\",\"x\":\"y\",\"a\":8}", ["hello"; "x"; "a"], "{\"a\": 8, \"x\": \"y\", \"hello\": \"world\"}");
  ("with selector with capital letters and values", "{\"myKey\":\"myVal\"}", ["myKey"], "{\"myKey\": \"myVal\", \"Merp\": 19}");
  ("with nested objects", "{\"myKey.myNestedKey\":\"myVal\"}", ["myKey.myNestedKey"], "{\"myKey\": {\"myNestedKey\": \"myVal\"}}");
]

let suite =
  "selector integration test suite" >::: List.map (fun (reason, expected, selector_strings, raw) ->
      let to_selector s = Selector s in
      reason >:: (fun ctxt -> assert_equal ~ctxt:ctxt
                     ~printer:(fun x -> x)
                     expected
                     (exp_output (Exp (
                          None,
                          List.map to_selector selector_strings)) raw))
    ) cases
