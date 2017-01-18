open OUnit2
open Grammar
open Eval

let msg_contains_hello = Pred (Field "msg", Contains [String "hello"])
let age_more_than_9 = Pred (Field "age", GT 9.0)
let name_is_john_smith = Pred (Field "name", Equal [(String "john smith")]) 
let level_is_error = Pred (Field "level", Equal [(String "error")])
let cases = [
  ("age is 9", (Some (Pred ((Field "age"), (Equal [(Num 9.0); (String "9")])))));
  ("age = 9", (Some (Pred ((Field "age"), (Equal [(Num 9.0); (String "9")])))));
  ("age > 9", (Some (Pred ((Field "age"), (GT 9.0)))));
  ("age is greater than 9", (Some (Pred ((Field "age"), (GT 9.0)))));
  ("age greater than 9", (Some (Pred ((Field "age"), (GT 9.0)))));
  ("age matches /^bob(by)?/", (Some (Pred ((Field "age"), (Matches (Regex "^bob(by)?"))))));
  ("age < -10", (Some (Pred ((Field "age"), (LT (-10.0))))));
  ("age is less than .09", (Some (Pred ((Field "age"), (LT (0.09))))));
  ("age less than 9.56", (Some (Pred ((Field "age"), (LT 9.56)))));
  ("user_role ends with \"end user\"", Some (Pred ((Field "user_role"), (EndsWith "end user"))));
  ("state is \"colorado\"", (Some (Pred ((Field "state"), (Equal [(String "colorado")])))));
  ("state is colorado", (Some (Pred ((Field "state"), (Equal [(String "colorado")])))));
  ("age greater than hello", None);
  ("msg contains Hello", (Some (Pred ((Field "msg"), (Contains [String "Hello"])))));
  ("msg contains hello and age > 9", (Some (And (Pred (Field "msg", Contains [String "hello"]), Pred (Field "age", GT 9.0)))));
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
  ("msg contains 'started fetching'", Some (Pred (Field "msg", Contains [String "started fetching"])));
  ("has field msg", Some (Pred (Field "msg", HasField)));
  ("has field \"last-name\"", Some (Pred (Field "last-name", HasField)));
  ("has key \"last-name\"", Some (Pred (Field "last-name", HasField)));
  ("statusCode is greater than 400 and has field 'lastName'", Some (And (Pred (Field "statusCode", GT 400.0), Pred (Field "lastName", HasField))));
  ("priority is not high", Some (Pred (Field "priority", Not (Equal [(String "high")]))));
  ("priority is not greater than 7", Some (Pred (Field "priority", Not (GT 7.0))));
  ("priority isn't greater than 7", Some (Pred (Field "priority", Not (GT 7.0))));
  ("does not have key priority", Some (Pred (Field "priority", Not HasField)));
  ("doesn't have key priority", Some (Pred (Field "priority", Not HasField)));
  ("subject does not begin with \"hello world\"", Some (Pred (Field "subject", Not (BeginsWith "hello world"))));
  ("subject doesn't begin with \"hello-world\"", Some (Pred (Field "subject", Not (BeginsWith "hello-world"))));
  ("msg doesn't contain request", Some (Pred (Field "msg", Not (Contains [String "request"]))));
  ("hello is t", Some (Pred (Field "hello", Equal [String "t"; Bool true])));
  ("hello is f", Some (Pred (Field "hello", Equal [String "f"; Bool false])));
  ("hello is false", Some (Pred (Field "hello", Equal [String "false"; Bool false])));
  (
    "ct > 55 && hello is false",
    Some (And (Pred (Field "ct", GT 55.0), Pred (Field "hello", Equal [String "false"; Bool false])))
  );
  ("hello starts with 9", Some (Pred (Field "hello", BeginsWith "9")));
  ("hello ends with 9", Some (Pred (Field "hello", EndsWith "9")));
  ("hello matches 9", Some (Pred (Field "hello",  Matches (Regex "9"))));
  ("hello matches world", Some (Pred (Field "hello",  Matches (Regex "world"))));
  ("hello contains 9", Some (Pred (Field "hello", Contains [Num 9.0; String "9"])));
  ("hello-world contains 9", Some (Pred (Field "hello-world", Contains [Num 9.0; String "9"])));
]
let suite = "filter parsing suite" >::: List.map (fun (raw, expected) ->
    raw >:: (fun ctxt -> assert_equal (parse_filter raw) expected ~ctxt)
  ) cases
