open! Core
open! Import
open! Regex_dfa.For_testing

let to_dfa = Regex_dfa.create ~cont_of_match:(Fn.const ())

let%expect_test "exact dfa" =
  let dfa = Regex_config.exact "foo " |> to_dfa in
  print_s [%message (dfa : unit t)];
  [%expect
    {|
    (dfa
     ((16 (Node (next_nodes ((f 17))))) (17 (Node (next_nodes ((o 18)))))
      (18 (Node (next_nodes ((o 19))))) (19 (Node (next_nodes ((" " 20)))))
      (20
       (Accept (next_nodes ())
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))))
    |}]
;;

let%expect_test "or dfa" =
  let dfa = Regex_config.char_or [ 'b'; 'a'; 'r' ] |> to_dfa in
  print_s [%message (dfa : unit t)];
  [%expect
    {|
    (dfa
     ((21 (Node (next_nodes ((a 22) (b 22) (r 22)))))
      (22
       (Accept (next_nodes ())
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))))
    |}]
;;

let%expect_test "complex dfa: floats" =
  let dfa = to_dfa Util.Common_config.float in
  print_s [%message (dfa : unit t)];
  [%expect
    {|
    (dfa
     ((23
       (Node
        (next_nodes
         ((0 24) (1 24) (2 24) (3 24) (4 24) (5 24) (6 24) (7 24) (8 24) (9 24)))))
      (24
       (Node
        (next_nodes
         ((. 25) (0 24) (1 24) (2 24) (3 24) (4 24) (5 24) (6 24) (7 24)
          (8 24) (9 24)))))
      (25
       (Accept
        (next_nodes
         ((0 25) (1 25) (2 25) (3 25) (4 25) (5 25) (6 25) (7 25) (8 25) (9 25)))
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))))
    |}]
;;

let%expect_test "complex dfa: phone" =
  let dfa = to_dfa Util.Common_config.phone in
  print_s [%message (dfa : unit t)];
  [%expect
    {|
    (dfa
     ((26
       (Node
        (next_nodes
         (("(" 27) (+ 41) (0 44) (1 44) (2 44) (3 44) (4 44) (5 44) (6 44)
          (7 44) (8 44) (9 44)))))
      (27
       (Node
        (next_nodes
         ((0 28) (1 28) (2 28) (3 28) (4 28) (5 28) (6 28) (7 28) (8 28) (9 28)))))
      (28
       (Node
        (next_nodes
         ((0 29) (1 29) (2 29) (3 29) (4 29) (5 29) (6 29) (7 29) (8 29) (9 29)))))
      (29
       (Node
        (next_nodes
         ((0 30) (1 30) (2 30) (3 30) (4 30) (5 30) (6 30) (7 30) (8 30) (9 30)))))
      (30 (Node (next_nodes ((")" 31))))) (31 (Node (next_nodes ((" " 32)))))
      (32
       (Node
        (next_nodes
         ((0 33) (1 33) (2 33) (3 33) (4 33) (5 33) (6 33) (7 33) (8 33) (9 33)))))
      (33
       (Node
        (next_nodes
         ((0 34) (1 34) (2 34) (3 34) (4 34) (5 34) (6 34) (7 34) (8 34) (9 34)))))
      (34
       (Node
        (next_nodes
         ((0 35) (1 35) (2 35) (3 35) (4 35) (5 35) (6 35) (7 35) (8 35) (9 35)))))
      (35 (Node (next_nodes ((- 36)))))
      (36
       (Node
        (next_nodes
         ((0 37) (1 37) (2 37) (3 37) (4 37) (5 37) (6 37) (7 37) (8 37) (9 37)))))
      (37
       (Node
        (next_nodes
         ((0 38) (1 38) (2 38) (3 38) (4 38) (5 38) (6 38) (7 38) (8 38) (9 38)))))
      (38
       (Node
        (next_nodes
         ((0 39) (1 39) (2 39) (3 39) (4 39) (5 39) (6 39) (7 39) (8 39) (9 39)))))
      (39
       (Node
        (next_nodes
         ((0 40) (1 40) (2 40) (3 40) (4 40) (5 40) (6 40) (7 40) (8 40) (9 40)))))
      (40
       (Accept (next_nodes ())
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))
      (41
       (Node
        (next_nodes
         ((0 42) (1 42) (2 42) (3 42) (4 42) (5 42) (6 42) (7 42) (8 42) (9 42)))))
      (42
       (Node
        (next_nodes
         ((" " 43) (0 42) (1 42) (2 42) (3 42) (4 42) (5 42) (6 42) (7 42)
          (8 42) (9 42)))))
      (43
       (Node
        (next_nodes
         (("(" 27) (0 44) (1 44) (2 44) (3 44) (4 44) (5 44) (6 44) (7 44)
          (8 44) (9 44)))))
      (44
       (Node
        (next_nodes
         ((0 45) (1 45) (2 45) (3 45) (4 45) (5 45) (6 45) (7 45) (8 45) (9 45)))))
      (45
       (Node
        (next_nodes
         ((0 46) (1 46) (2 46) (3 46) (4 46) (5 46) (6 46) (7 46) (8 46) (9 46)))))
      (46
       (Node
        (next_nodes
         ((- 32) (0 47) (1 47) (2 47) (3 47) (4 47) (5 47) (6 47) (7 47)
          (8 47) (9 47)))))
      (47
       (Node
        (next_nodes
         ((0 48) (1 48) (2 48) (3 48) (4 48) (5 48) (6 48) (7 48) (8 48) (9 48)))))
      (48
       (Node
        (next_nodes
         ((0 36) (1 36) (2 36) (3 36) (4 36) (5 36) (6 36) (7 36) (8 36) (9 36)))))))
    |}]
;;
