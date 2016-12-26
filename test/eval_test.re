open OUnit2;

open Grammar;

open Eval;

let module Parse = {
  let test1 ctx =>
    assert_equal (parse_filter "age is 9") (Some (Pred (Field "age") (Equal (Num 9.0))));
  let test2 ctx =>
    assert_equal (parse_filter "age = 9") (Some (Pred (Field "age") (Equal (Num 9.0))));
  let test3 ctx => assert_equal (parse_filter "age > 9") (Some (Pred (Field "age") (GT 9.0)));
  let test4 ctx =>
    assert_equal (parse_filter "age is greater than 9") (Some (Pred (Field "age") (GT 9.0)));
  let test5 ctx =>
    assert_equal (parse_filter "age greater than 9") (Some (Pred (Field "age") (GT 9.0)));
  let test6 ctx =>
    assert_equal
      (parse_filter "age matches /^bob(by)?/")
      (Some (Pred (Field "age") (Matches (Regex "^bob(by)?"))));
  let test7 ctx =>
    assert_equal (parse_filter "age < -10") (Some (Pred (Field "age") (LT (-10.0))));
  let test8 ctx =>
    assert_equal (parse_filter "age is less than .09") (Some (Pred (Field "age") (LT 0.09)));
  let test9 ctx =>
    assert_equal (parse_filter "age less than 9.56") (Some (Pred (Field "age") (LT 9.56)));
  let test10 ctx =>
    assert_equal
      (parse_filter "user_role ends with \"end user\"")
      (Some (Pred (Field "user_role") (EndsWith "end user")));
  let test11 ctx =>
    assert_equal
      (parse_filter "state is \"colorado\"")
      (Some (Pred (Field "state") (Equal (String "colorado"))));
  let test12 ctx =>
    assert_equal
      (parse_filter "state is colorado") (Some (Pred (Field "state") (Equal (String "colorado"))));
  let test13 ctx => assert_equal (parse_filter "age greater than hello") None;
  let test14 ctx =>
    assert_equal
      (parse_filter "msg contains Hello") (Some (Pred (Field "msg") (Contains "Hello")));
  let test15 ctx =>
    assert_equal
      printer::[%show : option predicate]
      (parse_filter "msg contains hello and age > 9")
      (Some (And (Pred (Field "msg") (Contains "hello")) (Pred (Field "age") (GT 9.0))));
  let msgContainsHello = Pred (Field "msg") (Contains "hello");
  let ageMoreThan9 = Pred (Field "age") (GT 9.0);
  let nameIsJohnSmith = Pred (Field "name") (Equal (String "john smith"));
  let levelIsError = Pred (Field "level") (Equal (String "error"));
  let test16 ctx =>
    assert_equal
      printer::[%show : option predicate]
      (parse_filter "msg contains hello and age > 9 and name is \"john smith\"")
      (Some (And msgContainsHello (And ageMoreThan9 nameIsJohnSmith)));
  let test17 ctx =>
    assert_equal
      printer::[%show : option predicate]
      (parse_filter "(msg contains hello)")
      (Some msgContainsHello);
  let test18 ctx =>
    assert_equal
      printer::[%show : option predicate]
      (Some (Or (And msgContainsHello ageMoreThan9) (And nameIsJohnSmith levelIsError)))
      (
        parse_filter "(msg contains hello and age > 9) or (name is \"john smith\" and level is error)"
      );
  let test19 ctx =>
    assert_equal
      (Some (And msgContainsHello (Or ageMoreThan9 (And nameIsJohnSmith levelIsError))))
      (parse_filter "msg contains hello and age > 9 or name is \"john smith\" and level is error");
  let suite =
    "filter parsing suite" >::: [
      "test1" >:: test1,
      "test2" >:: test2,
      "test3" >:: test3,
      "test4" >:: test4,
      "test5" >:: test5,
      "test6" >:: test6,
      "test7" >:: test7,
      "test8" >:: test8,
      "test9" >:: test9,
      "test10" >:: test10,
      "test11" >:: test11,
      "test12" >:: test12,
      "test13" >:: test13,
      "test14" >:: test14,
      "test15" >:: test15,
      "test16" >:: test16,
      "test17" >:: test17,
      "test18" >:: test18,
      "test19" >:: test19
    ];
};
