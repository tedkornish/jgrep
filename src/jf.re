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
      switch (Filters.parseFilter s) {
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
  while true {
    switch (readLine (), initialState.exp) {
    | (Process s, None) => print_endline s
    | (Process s, Some e) =>
      print_endline s;
      if (s != "" && Filters.passesFilter e s) {
        print_endline s
      }
    | (End, _) => exit 0
    }
  };
  ()
};

main ();
