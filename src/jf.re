open Sys;

type rawState = {filterString: string};

type initialState = {exp: option Grammar.exp};

let rawStateFromArgs () :rawState => {
  let filterString = ref "";
  let speclist = [("-filter", Arg.Set_string filterString, "filters")];
  let _ = Arg.parse speclist (fun _ => ()) "A program for parsing JSON logs.";
  {filterString: !filterString}
};

let parseRawState (raw: rawState) :initialState => {
  let filterExp =
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
  {exp: filterExp}
};

type lineAction =
  | End
  | Process string;

let readLine () =>
  try (Process (read_line ())) {
  | End_of_file => End
  };

let main () => {
  let initialState = rawStateFromArgs () |> parseRawState;
  let _ = signal sigint (Signal_handle (fun _ => exit 0));
  let (proc, predicate) =
    switch initialState.exp {
    | None => (None, (fun _ => true))
    | Some exp =>
      let proc = Eval.newProcess exp;
      (Some proc, Eval.passesFilter proc)
    };
  while true {
    switch (readLine (), initialState.exp) {
    | (Process s, None) => print_endline s
    | (Process s, Some _) =>
      if (s != "" && predicate s) {
        print_endline s
      }
    | (End, _) =>
      switch proc {
      | Some p => Eval.closeProcess p |> (fun _ => ())
      | None => ()
      };
      exit 0
    }
  };
  ()
};

main ();
