open OUnit2;

let run (cmd: string) :string => {
  let inp = Unix.open_process_in cmd;
  let r = input_line inp;
  close_in inp;
  r
};

let test1 ctx =>
  assert_equal (run "echo '{\"age\": 9, \"hello\": \"world\"}' | jq '.age > 5'") "true";

let test2 ctx =>
  assert_equal
    (run "echo '{\"age\": 9, \"hello\": \"world\"}' | jq '(.age > 5 and .hello == \"world\")'")
    "true";

let suite = "jq programs suite" >::: ["test1" >:: test1, "test2" >:: test2];
