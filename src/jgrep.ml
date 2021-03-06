open Sys

type raw_state = {
  filter_string : string;
  selector_string : string;
  file : string;
  tsv : bool;
  dry : bool
}

type parsed_state = {
  exp : Grammar.exp;
  file : string option;
  tsv : bool;
  dry : bool
}

let raw_state_from_args () :raw_state =
  let filter_string = ref "" in
  let selector_string = ref "" in
  let file_string = ref "" in
  let tsv_bool = ref false in
  let dry_bool = ref false in
  let speclist = [
    ("-filter", Arg.Set_string filter_string, "filters");
    ("-select", Arg.Set_string selector_string, "selectors");
    ("-file", Arg.Set_string file_string, "read from a file instead of stdin");
    ("-tsv", Arg.Bool (fun b -> tsv_bool := b), "output as a tsv instead of JSON");
    ("-dry", Arg.Bool (fun b -> dry_bool := b), "print out jq script")
  ] in
  let _ = Arg.parse speclist (fun _ -> ()) "A program for parsing JSON logs" in
  {
    filter_string = !filter_string;
    selector_string = !selector_string;
    file = !file_string;
    tsv = !tsv_bool;
    dry = !dry_bool;
  }

let parse_raw_state (raw : raw_state) :parsed_state =
  let predicate = match raw.filter_string with
    | "" -> None
    | s -> match (Eval.parse_filter s) with
      | Some exp -> Some exp
      | None -> prerr_endline "invalid filter"; exit 1
  in let selectors = match raw.selector_string with
      | "" -> []
      | s -> Str.split (Str.regexp ",") s |> List.map (fun x -> Grammar.Selector x)
  in {
    exp = Grammar.Exp (predicate, selectors);
    file = (match raw.file with "" -> None | f -> Some f);
    tsv = raw.tsv;
    dry = raw.dry;
  }

type line_action = End | Process of string

let process_line input = try (Process (input_line input)) with End_of_file -> End

let open_file_opt filename = match filename with
  | None -> None
  | Some f -> Some (open_in f)

let close_file_opt input = match input with
  | None -> ()
  | Some i -> close_in i

let get_input_channel input = match input with
  | None -> stdin
  | Some i -> i

let main () =
  let raw = raw_state_from_args () in
  let parsed = parse_raw_state raw in
  let () = (match parsed.dry with
    | true -> print_endline (Eval.to_jq ~tsv:parsed.tsv parsed.exp); exit 0
    | false -> ()) in
  let _ = signal sigint (Signal_handle (fun _ -> exit 0)) in
  let proc = Eval.new_process ~tsv:parsed.tsv parsed.exp in
  let input = open_file_opt parsed.file |> get_input_channel in
  while true do
    match (process_line input) with
    | Process s ->
      let processed = Eval.process_line proc s in
      (match processed with "{}" -> () | _ -> print_endline processed; flush stdout)
    | End -> Eval.close_process proc; exit 0
  done

let () = main ();
