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
     ((422 (Node (next_nodes ((f 423))))) (423 (Node (next_nodes ((o 424)))))
      (424 (Node (next_nodes ((o 425))))) (425 (Node (next_nodes ((" " 426)))))
      (426
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
     ((427 (Node (next_nodes ((a 428) (b 428) (r 428)))))
      (428
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
     ((429
       (Node
        (next_nodes
         ((0 430) (1 430) (2 430) (3 430) (4 430) (5 430) (6 430) (7 430)
          (8 430) (9 430)))))
      (430
       (Node
        (next_nodes
         ((. 431) (0 430) (1 430) (2 430) (3 430) (4 430) (5 430) (6 430)
          (7 430) (8 430) (9 430)))))
      (431
       (Accept
        (next_nodes
         ((0 431) (1 431) (2 431) (3 431) (4 431) (5 431) (6 431) (7 431)
          (8 431) (9 431)))
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))))
    |}]
;;

let%expect_test "complex dfa: phone" =
  let dfa = to_dfa Util.Common_config.phone in
  print_s [%message (dfa : unit t)];
  [%expect
    {|
    (dfa
     ((432
       (Node
        (next_nodes
         (("(" 433) (+ 447) (0 450) (1 450) (2 450) (3 450) (4 450) (5 450)
          (6 450) (7 450) (8 450) (9 450)))))
      (433
       (Node
        (next_nodes
         ((0 434) (1 434) (2 434) (3 434) (4 434) (5 434) (6 434) (7 434)
          (8 434) (9 434)))))
      (434
       (Node
        (next_nodes
         ((0 435) (1 435) (2 435) (3 435) (4 435) (5 435) (6 435) (7 435)
          (8 435) (9 435)))))
      (435
       (Node
        (next_nodes
         ((0 436) (1 436) (2 436) (3 436) (4 436) (5 436) (6 436) (7 436)
          (8 436) (9 436)))))
      (436 (Node (next_nodes ((")" 437))))) (437 (Node (next_nodes ((" " 438)))))
      (438
       (Node
        (next_nodes
         ((0 439) (1 439) (2 439) (3 439) (4 439) (5 439) (6 439) (7 439)
          (8 439) (9 439)))))
      (439
       (Node
        (next_nodes
         ((0 440) (1 440) (2 440) (3 440) (4 440) (5 440) (6 440) (7 440)
          (8 440) (9 440)))))
      (440
       (Node
        (next_nodes
         ((0 441) (1 441) (2 441) (3 441) (4 441) (5 441) (6 441) (7 441)
          (8 441) (9 441)))))
      (441 (Node (next_nodes ((- 442)))))
      (442
       (Node
        (next_nodes
         ((0 443) (1 443) (2 443) (3 443) (4 443) (5 443) (6 443) (7 443)
          (8 443) (9 443)))))
      (443
       (Node
        (next_nodes
         ((0 444) (1 444) (2 444) (3 444) (4 444) (5 444) (6 444) (7 444)
          (8 444) (9 444)))))
      (444
       (Node
        (next_nodes
         ((0 445) (1 445) (2 445) (3 445) (4 445) (5 445) (6 445) (7 445)
          (8 445) (9 445)))))
      (445
       (Node
        (next_nodes
         ((0 446) (1 446) (2 446) (3 446) (4 446) (5 446) (6 446) (7 446)
          (8 446) (9 446)))))
      (446
       (Accept (next_nodes ())
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))
      (447
       (Node
        (next_nodes
         ((0 448) (1 448) (2 448) (3 448) (4 448) (5 448) (6 448) (7 448)
          (8 448) (9 448)))))
      (448
       (Node
        (next_nodes
         ((" " 449) (0 448) (1 448) (2 448) (3 448) (4 448) (5 448) (6 448)
          (7 448) (8 448) (9 448)))))
      (449
       (Node
        (next_nodes
         (("(" 433) (0 450) (1 450) (2 450) (3 450) (4 450) (5 450) (6 450)
          (7 450) (8 450) (9 450)))))
      (450
       (Node
        (next_nodes
         ((0 451) (1 451) (2 451) (3 451) (4 451) (5 451) (6 451) (7 451)
          (8 451) (9 451)))))
      (451
       (Node
        (next_nodes
         ((0 452) (1 452) (2 452) (3 452) (4 452) (5 452) (6 452) (7 452)
          (8 452) (9 452)))))
      (452
       (Node
        (next_nodes
         ((- 438) (0 453) (1 453) (2 453) (3 453) (4 453) (5 453) (6 453)
          (7 453) (8 453) (9 453)))))
      (453
       (Node
        (next_nodes
         ((0 454) (1 454) (2 454) (3 454) (4 454) (5 454) (6 454) (7 454)
          (8 454) (9 454)))))
      (454
       (Node
        (next_nodes
         ((0 442) (1 442) (2 442) (3 442) (4 442) (5 442) (6 442) (7 442)
          (8 442) (9 442)))))))
    |}]
;;
