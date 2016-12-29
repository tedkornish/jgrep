type regex = Regex of string [@@deriving show]

type value = Bool of bool | String of string | Num of float [@@deriving show]

(* filters on string values, include Matches, should be case-insensitive *)
type filter =
  | GT of float
  | LT of float
  | Equal of value
  | Matches of regex
  | Contains of string
  | BeginsWith of string
  | EndsWith of string
  | Not of filter
[@@deriving show]

type field = Field of string [@@deriving show]

type predicate =
  | Pred of field * filter
  | And of predicate * predicate
  | Or of predicate * predicate
[@@deriving show]

type selector = Selector of string

type exp = Exp of (predicate option) * (selector list)
