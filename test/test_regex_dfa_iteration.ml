open! Core
open! Import
open! Regex_dfa.For_testing

let to_dfa_iterator config =
  Regex_dfa.create config ~cont_of_match:Buffer.contents |> Regex_dfa.Iterator.make
;;

let print_next_result ?(silent = false) iterator c =
  let result = Regex_dfa.Iterator.next iterator ~c:(Some c) in
  if not silent then print_s [%message (result : string Regex_dfa.Iterator.Result.t)]
;;

let%expect_test "exact dfa (success)" =
  let iterator = Regex_config.exact "foo " |> to_dfa_iterator in
  print_next_result iterator 'f';
  [%expect {| (result (Incomplete (is_accepting_state false))) |}];
  print_next_result iterator 'o';
  [%expect {| (result (Incomplete (is_accepting_state false))) |}];
  print_next_result iterator 'o';
  [%expect {| (result (Incomplete (is_accepting_state false))) |}];
  print_next_result iterator ' ';
  [%expect {| (result (Incomplete (is_accepting_state true))) |}];
  print_next_result iterator '\n';
  [%expect {| (result (Complete (result "foo ") (unused_len 1))) |}]
;;

let%expect_test "exact dfa (failure)" =
  let iterator () = Regex_config.exact "foo " |> to_dfa_iterator in
  print_next_result (iterator ()) 'z';
  [%expect {| (result (Failure (input z))) |}];
  let iterator = iterator () in
  print_next_result iterator 'f';
  [%expect {| (result (Incomplete (is_accepting_state false))) |}];
  print_next_result iterator 'o';
  [%expect {| (result (Incomplete (is_accepting_state false))) |}];
  print_next_result iterator 'o';
  [%expect {| (result (Incomplete (is_accepting_state false))) |}];
  print_next_result iterator 'o';
  [%expect {| (result (Failure (input fooo))) |}]
;;

let%expect_test "or dfa (success)" =
  let iterator () = Regex_config.char_or [ 'b'; 'a'; 'r' ] |> to_dfa_iterator in
  let () =
    let b_iterator = iterator () in
    print_next_result b_iterator 'b';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result b_iterator '\n';
    [%expect {| (result (Complete (result b) (unused_len 1))) |}]
  in
  let () =
    let a_iterator = iterator () in
    print_next_result a_iterator 'a';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result a_iterator '\n';
    [%expect {| (result (Complete (result a) (unused_len 1))) |}]
  in
  let () =
    let r_iterator = iterator () in
    print_next_result r_iterator 'r';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result r_iterator '\n';
    [%expect {| (result (Complete (result r) (unused_len 1))) |}]
  in
  ()
;;

let%expect_test "or dfa (failure)" =
  let iterator = Regex_config.char_or [ 'b'; 'a'; 'r' ] |> to_dfa_iterator in
  print_next_result iterator 'z';
  [%expect {| (result (Failure (input z))) |}]
;;

let%expect_test "complex dfa: long unused (success)" =
  let iterator =
    Regex_config.Or (Regex_config.exact "foo bar", Regex_config.exact "foo")
    |> to_dfa_iterator
  in
  print_next_result iterator 'f';
  [%expect {| (result (Incomplete (is_accepting_state false))) |}];
  print_next_result iterator 'o';
  [%expect {| (result (Incomplete (is_accepting_state false))) |}];
  print_next_result iterator 'o';
  [%expect {| (result (Incomplete (is_accepting_state true))) |}];
  print_next_result iterator ' ';
  [%expect {| (result (Incomplete (is_accepting_state false))) |}];
  print_next_result iterator 'b';
  [%expect {| (result (Incomplete (is_accepting_state false))) |}];
  print_next_result iterator 'a';
  [%expect {| (result (Incomplete (is_accepting_state false))) |}];
  print_next_result iterator 'h';
  [%expect {| (result (Complete (result foo) (unused_len 4))) |}]
;;

let%expect_test "complex dfa: floats (success)" =
  let iterator () = to_dfa_iterator Util.Common_config.float in
  let () =
    let pi_iterator = iterator () in
    print_next_result pi_iterator '3';
    [%expect {| (result (Incomplete (is_accepting_state false))) |}];
    print_next_result pi_iterator '.';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result pi_iterator '1';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result pi_iterator '4';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result pi_iterator '\n';
    [%expect {| (result (Complete (result 3.14) (unused_len 1))) |}]
  in
  let () =
    let int_iterator = iterator () in
    print_next_result int_iterator '1';
    [%expect {| (result (Incomplete (is_accepting_state false))) |}];
    print_next_result int_iterator '2';
    [%expect {| (result (Incomplete (is_accepting_state false))) |}];
    print_next_result int_iterator '3';
    [%expect {| (result (Incomplete (is_accepting_state false))) |}];
    print_next_result int_iterator '.';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result int_iterator '\n';
    [%expect {| (result (Complete (result 123.) (unused_len 1))) |}]
  in
  ()
;;

let%expect_test "complex dfa: floats (failure)" =
  let iterator () = to_dfa_iterator Util.Common_config.float in
  print_next_result (iterator ()) '.';
  [%expect {| (result (Failure (input .))) |}];
  let int_iterator = iterator () in
  print_next_result int_iterator '1';
  [%expect {| (result (Incomplete (is_accepting_state false))) |}];
  print_next_result int_iterator '2';
  [%expect {| (result (Incomplete (is_accepting_state false))) |}];
  print_next_result int_iterator '3';
  [%expect {| (result (Incomplete (is_accepting_state false))) |}];
  print_next_result int_iterator '\n';
  [%expect {| (result (Failure (input "123\n"))) |}]
;;

module Phone_util = struct
  let print_cc_result iterator =
    print_next_result ~silent:true iterator '+';
    print_next_result ~silent:true iterator '1';
    print_next_result ~silent:true iterator ' '
  ;;

  let print_group1_result iterator =
    print_next_result ~silent:true iterator '1';
    print_next_result ~silent:true iterator '2';
    print_next_result ~silent:true iterator '3'
  ;;

  let print_group2_result iterator =
    print_next_result ~silent:true iterator '4';
    print_next_result ~silent:true iterator '5';
    print_next_result ~silent:true iterator '6'
  ;;

  let print_group3_result iterator =
    print_next_result ~silent:true iterator '7';
    print_next_result ~silent:true iterator '8';
    print_next_result ~silent:true iterator '9';
    print_next_result ~silent:true iterator '0';
    print_next_result iterator '\n'
  ;;

  let print_straight_result iterator =
    print_group1_result iterator;
    print_group2_result iterator;
    print_group3_result iterator
  ;;

  let print_dash_result iterator =
    print_group1_result iterator;
    print_next_result iterator '-';
    print_group2_result iterator;
    print_next_result iterator '-';
    print_group3_result iterator
  ;;

  let print_paren_result iterator =
    print_next_result iterator '(';
    print_group1_result iterator;
    print_next_result iterator ')';
    print_next_result iterator ' ';
    print_group2_result iterator;
    print_next_result iterator '-';
    print_group3_result iterator
  ;;
end

let%expect_test "complex dfa: phone (success)" =
  let iterator () = to_dfa_iterator Util.Common_config.phone in
  let () =
    let straight_with_cc_iterator = iterator () in
    Phone_util.print_cc_result straight_with_cc_iterator;
    Phone_util.print_straight_result straight_with_cc_iterator;
    [%expect {| (result (Complete (result "+1 1234567890") (unused_len 1))) |}];
    let straight_iterator = iterator () in
    Phone_util.print_straight_result straight_iterator;
    [%expect {| (result (Complete (result 1234567890) (unused_len 1))) |}]
  in
  let () =
    let dash_with_cc_iterator = iterator () in
    Phone_util.print_cc_result dash_with_cc_iterator;
    Phone_util.print_dash_result dash_with_cc_iterator;
    [%expect
      {|
      (result (Incomplete (is_accepting_state false)))
      (result (Incomplete (is_accepting_state false)))
      (result (Complete (result "+1 123-456-7890") (unused_len 1)))
      |}];
    let dash_iterator = iterator () in
    Phone_util.print_dash_result dash_iterator;
    [%expect
      {|
      (result (Incomplete (is_accepting_state false)))
      (result (Incomplete (is_accepting_state false)))
      (result (Complete (result 123-456-7890) (unused_len 1)))
      |}]
  in
  let () =
    let paren_with_cc_iterator = iterator () in
    Phone_util.print_cc_result paren_with_cc_iterator;
    Phone_util.print_paren_result paren_with_cc_iterator;
    [%expect
      {|
      (result (Incomplete (is_accepting_state false)))
      (result (Incomplete (is_accepting_state false)))
      (result (Incomplete (is_accepting_state false)))
      (result (Incomplete (is_accepting_state false)))
      (result (Complete (result "+1 (123) 456-7890") (unused_len 1)))
      |}];
    let paren_iterator = iterator () in
    Phone_util.print_paren_result paren_iterator;
    [%expect
      {|
      (result (Incomplete (is_accepting_state false)))
      (result (Incomplete (is_accepting_state false)))
      (result (Incomplete (is_accepting_state false)))
      (result (Incomplete (is_accepting_state false)))
      (result (Complete (result "(123) 456-7890") (unused_len 1)))
      |}]
  in
  ()
;;

let%expect_test "complex dfa: phone (failure)" =
  let iterator () = to_dfa_iterator Util.Common_config.phone in
  let () =
    let bad_cc_iterator = iterator () in
    print_next_result ~silent:true bad_cc_iterator '+';
    print_next_result bad_cc_iterator '(';
    [%expect {| (result (Failure (input "+("))) |}]
  in
  let () =
    let straight_dash_with_cc_iterator = iterator () in
    Phone_util.print_cc_result straight_dash_with_cc_iterator;
    Phone_util.print_group1_result straight_dash_with_cc_iterator;
    Phone_util.print_group2_result straight_dash_with_cc_iterator;
    print_next_result straight_dash_with_cc_iterator '-';
    [%expect {| (result (Failure (input "+1 123456-"))) |}]
  in
  let () =
    let no_space_paren_with_cc_iterator = iterator () in
    Phone_util.print_cc_result no_space_paren_with_cc_iterator;
    print_next_result ~silent:true no_space_paren_with_cc_iterator '(';
    Phone_util.print_group1_result no_space_paren_with_cc_iterator;
    print_next_result ~silent:true no_space_paren_with_cc_iterator ')';
    print_next_result no_space_paren_with_cc_iterator '4';
    [%expect {| (result (Failure (input "+1 (123)4"))) |}]
  in
  ()
;;
