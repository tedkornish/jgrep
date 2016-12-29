open Eval

let exp_output exp json =
  let p = new_process exp in
  let line = process_line p json in
  let _ = close_process p in
  line
