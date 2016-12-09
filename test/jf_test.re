open OUnit2;

let _ = {
  let _ = run_test_tt_main Filters_test.Parse.suite;
  let _ = run_test_tt_main Jq_test.suite;
  let _ = run_test_tt_main Integration_test.suite;
  ()
};
