open! Core
open! Import
open Regex_config.For_testing

let%expect_test "exact possible chars" =
  let config = Regex_config.exact "foo " in
  print_s [%message (possible_chars config : Char.Set.t)];
  [%expect {| ("possible_chars config" (" " f o)) |}]
;;

let%expect_test "or possible chars" =
  let config = Regex_config.char_or [ 'b'; 'a'; 'r' ] in
  print_s [%message (possible_chars config : Char.Set.t)];
  [%expect {| ("possible_chars config" (a b r)) |}]
;;

let%expect_test "complex possible chars: floats" =
  let config = Util.Common_config.float in
  print_s [%message (possible_chars config : Char.Set.t)];
  [%expect {| ("possible_chars config" (. 0 1 2 3 4 5 6 7 8 9)) |}]
;;

let%expect_test "complex possible chars: phone" =
  let config = Util.Common_config.phone in
  print_s [%message (possible_chars config : Char.Set.t)];
  [%expect {| ("possible_chars config" (" " "(" ")" + - 0 1 2 3 4 5 6 7 8 9)) |}]
;;
