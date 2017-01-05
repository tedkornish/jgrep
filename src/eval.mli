(** This module houses functions which evaluates an expression AST into a
    small jq program, plus functions which execute that jq program and pipe
    input through it. *)

(** This is the raison d'etre for the Eval module: this function turns an
    expression with a predicate tree and selectors into a fully-formed jq
    program. The program acts on a single JSON object, performing the following
    computation:

    1. Try to apply the filter produced by the predicate tree

    2. If the filter passes, select out a subset of keys based on any present selectors; format either as a JSON document or a TSV line

    3. Else, return an empty object

    The string which is returned from this function is ready to be used in bash
    to spin up a new process for filtering and selecting from JSON objects.
*)
val to_jq : Grammar.exp -> tsv:bool -> string

(** [jq_process] represents an open process which is running a JQ command. The
    type contains the full bash command itself, an [in_channel] to stdin, and an
    [out_channel] reading from stdout. *)
type jq_process

(** Create a jq process for a given expression by turning it into a jq command
    and starting the command as a Unix subprocess, returning an object used to
    access that process. *)
val new_process : ?tsv:bool -> Grammar.exp -> jq_process

(** For a given running [jq_process], pass in a string - which should be a
    fully-formed JSON blob - and capture the output. *)
val process_line : jq_process -> string -> string

(** Close the input and output channels to a [jq_process]. *)
val close_process : jq_process -> unit

(** Try to parse a filter from a given string; return [None] if no successful
    parse. *)
val parse_filter : string -> Grammar.predicate option

(** If an input line fails to process properly against JQ, it raises a
    [Jq_error]. The three strings are, in order:
    
    1. The error message from jq

    2. The compiler [jq] command that initiated the [jq_process]
    
    3. The line of JSON input that triggered the error
*)
exception Jq_error of string * string * string
