let () = OUnit2.run_test_tt_main Eval_test.Parse.suite
let () = OUnit2.run_test_tt_main Integration_test.Predicate.suite
let () = OUnit2.run_test_tt_main Integration_test.Exp.suite
