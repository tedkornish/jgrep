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

type predicate =
  | Pred field filter
  | And predicate predicate
  | Or predicate predicate
[@@deriving show];

type selector =
  | Selector string;

type exp =
  | Exp predicate (list selector);
