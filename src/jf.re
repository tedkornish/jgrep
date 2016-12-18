open Sys;

type rawState = {filterString: string, selectorString: string};

let rawStateFromArgs () :rawState => {
  let filterString = ref "";
  let selectorString = ref "";
  let speclist = [
    ("-filter", Arg.Set_string filterString, "filters"),
    ("-select", Arg.Set_string selectorString, "selectors")
  ];
  let _ = Arg.parse speclist (fun _ => ()) "A program for parsing JSON logs.";
  {filterString: !filterString, selectorString: !selectorString}
};

let parseRawState (raw: rawState) :Grammar.exp => {
  let predicate =
    switch raw.filterString {
    | "" => None
    | s =>
      switch (Eval.parseFilter s) {
      | Some exp => Some exp
      | None =>
        prerr_endline "invalid filter";
        exit 1 /* TODO better err handling */
      }
    };
  let selectors =
    switch raw.selectorString {
    | "" => []
    | s => Str.split (Str.regexp ",") s |> List.map (fun x => Grammar.Selector x)
    };
  Grammar.Exp predicate selectors
};

type lineAction =
  | End
  | Process string;

let readLine () =>
  try (Process (read_line ())) {
  | End_of_file => End
  };

let main () => {
  let exp = rawStateFromArgs () |> parseRawState;
  let _ = signal sigint (Signal_handle (fun _ => exit 0));
  let proc = Eval.newProcess exp;
  while true {
    switch (readLine ()) {
    | Process s =>
      let processed = Eval.processLine proc s;
      if (processed != "{}") {
        print_endline processed
      }
    | End => Eval.closeProcess proc |> (fun _ => exit 0)
    }
  };
  ()
};

main ();
