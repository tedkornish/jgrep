open OUnit2;

let _ = {
  let _ = run_test_tt_main Eval_test.Parse.suite;
  let _ = run_test_tt_main Integration_test.Predicate.suite;
  let _ = run_test_tt_main Integration_test.Exp.suite;
  ()
};
