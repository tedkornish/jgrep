open OUnit2
open Grammar
open Eval

module Parse = struct
  let msg_contains_hello = Pred (Field "msg", Contains "hello")
  let age_more_than_9 = Pred (Field "age", GT 9.0)
  let name_is_john_smith = Pred (Field "name", Equal (String "john smith"))
  let level_is_error = Pred (Field "level", Equal (String "error"))
  let cases = [
    ("age is 9", (Some (Pred ((Field "age"), (Equal (Num 9.0))))));
    ("age = 9", (Some (Pred ((Field "age"), (Equal (Num 9.0))))));
    ("age > 9", (Some (Pred ((Field "age"), (GT 9.0)))));
    ("age is greater than 9", (Some (Pred ((Field "age"), (GT 9.0)))));
    ("age greater than 9", (Some (Pred ((Field "age"), (GT 9.0)))));
    ("age matches /^bob(by)?/", (Some (Pred ((Field "age"), (Matches (Regex "^bob(by)?"))))));
    ("age < -10", (Some (Pred ((Field "age"), (LT (-10.0))))));
    ("age is less than .09", (Some (Pred ((Field "age"), (LT (0.09))))));
    ("age less than 9.56", (Some (Pred ((Field "age"), (LT 9.56)))));
    ("user_role ends with \"end user\"", Some (Pred ((Field "user_role"), (EndsWith "end user"))));
    ("state is \"colorado\"", (Some (Pred ((Field "state"), (Equal (String "colorado"))))));
    ("state is colorado", (Some (Pred ((Field "state"), (Equal (String "colorado"))))));
    ("age greater than hello", None);
    ("msg contains Hello", (Some (Pred ((Field "msg"), (Contains "Hello")))));
    ("msg contains hello and age > 9", (Some (And (Pred (Field "msg", Contains "hello"), Pred (Field "age", GT 9.0)))));
    ("msg contains hello and age > 9 and name is \"john smith\"", Some (And (msg_contains_hello, (And (age_more_than_9, name_is_john_smith)))));
    ("(msg contains hello)", Some msg_contains_hello);
    ("((((msg contains hello))))", Some msg_contains_hello);
    (
      "(msg contains hello and age > 9) or (name is \"john smith\" and level is error)",
      Some (Or (And (msg_contains_hello, age_more_than_9), (And (name_is_john_smith, level_is_error))))
    );
    (
      "msg contains hello and age > 9 or name is \"john smith\" and level is error",
      Some (And (msg_contains_hello, (Or (age_more_than_9, (And (name_is_john_smith, level_is_error))))))
    );
  ]
  let suite = "filter parsing suite" >::: List.map (fun (raw, expected) ->
      raw >:: (fun ctxt -> assert_equal (parse_filter raw) expected ~ctxt:ctxt)
    ) cases
end
