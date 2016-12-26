open Sys

type raw_state = {filter_string : string; selector_string : string}

let raw_state_from_args () :raw_state =
  let filter_string = ref "" in
  let selector_string = ref "" in
  let speclist = [
    ("-filter", Arg.Set_string filter_string, "filters");
    ("-select", Arg.Set_string selector_string, "selectors")
  ] in
  let _ = Arg.parse speclist (fun _ -> ()) "A program for parsing JSON logs" in
  {filter_string = !filter_string; selector_string = !selector_string}

let parse_raw_state (raw : raw_state) :Grammar.exp =
  let predicate = match raw.filter_string with
    | "" -> None
    | s -> match (Eval.parse_filter s) with
      | Some exp -> Some exp
      | None -> prerr_endline "invalid filter"; exit 1
  in let selectors = match raw.selector_string with
    | "" -> []
    | s -> Str.split (Str.regexp ",") s |> List.map (fun x -> Grammar.Selector x)
  in Grammar.Exp (predicate, selectors)
    
type line_action = End | Process of string

let process_line () = try (Process (read_line ())) with End_of_file -> End

let main () =
  let exp = raw_state_from_args () |> parse_raw_state in
  let _ = signal sigint (Signal_handle (fun _ -> exit 0)) in
  let proc = Eval.new_process exp in
  while true do
    match (process_line ()) with
      | Process s ->
        let processed = Eval.process_line proc s in
        if (processed != "{}") then print_endline processed
      | End -> Eval.close_process proc; exit 0
  done

let () = main ();
