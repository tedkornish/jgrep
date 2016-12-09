type regex =
  | Regex string
[@@deriving show];

type value =
  | Bool bool
  | String string
  | Num float
[@@deriving show];

type filter =
  | GT float
  | LT float
  | Equal value
  | Matches regex
  | Contains string
  | BeginsWith string
  | EndsWith string
  | Not filter
[@@deriving show];

type field =
  | Field string
[@@deriving show];

type exp =
  | Exp field filter
  | And exp exp
  | Or exp exp
[@@deriving show];
