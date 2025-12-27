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
     ((571 (Node (next_nodes ((f 572))))) (572 (Node (next_nodes ((o 573)))))
      (573 (Node (next_nodes ((o 574))))) (574 (Node (next_nodes ((" " 575)))))
      (575
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
     ((576 (Node (next_nodes ((a 577) (b 577) (r 577)))))
      (577
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
     ((578
       (Node
        (next_nodes
         ((0 579) (1 579) (2 579) (3 579) (4 579) (5 579) (6 579) (7 579)
          (8 579) (9 579)))))
      (579
       (Node
        (next_nodes
         ((. 580) (0 579) (1 579) (2 579) (3 579) (4 579) (5 579) (6 579)
          (7 579) (8 579) (9 579)))))
      (580
       (Accept
        (next_nodes
         ((0 580) (1 580) (2 580) (3 580) (4 580) (5 580) (6 580) (7 580)
          (8 580) (9 580)))
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))))
    |}]
;;

let%expect_test "complex dfa: phone" =
  let dfa = to_dfa Util.Common_config.phone in
  print_s [%message (dfa : unit t)];
  [%expect
    {|
    (dfa
     ((581
       (Node
        (next_nodes
         (("(" 582) (+ 596) (0 599) (1 599) (2 599) (3 599) (4 599) (5 599)
          (6 599) (7 599) (8 599) (9 599)))))
      (582
       (Node
        (next_nodes
         ((0 583) (1 583) (2 583) (3 583) (4 583) (5 583) (6 583) (7 583)
          (8 583) (9 583)))))
      (583
       (Node
        (next_nodes
         ((0 584) (1 584) (2 584) (3 584) (4 584) (5 584) (6 584) (7 584)
          (8 584) (9 584)))))
      (584
       (Node
        (next_nodes
         ((0 585) (1 585) (2 585) (3 585) (4 585) (5 585) (6 585) (7 585)
          (8 585) (9 585)))))
      (585 (Node (next_nodes ((")" 586))))) (586 (Node (next_nodes ((" " 587)))))
      (587
       (Node
        (next_nodes
         ((0 588) (1 588) (2 588) (3 588) (4 588) (5 588) (6 588) (7 588)
          (8 588) (9 588)))))
      (588
       (Node
        (next_nodes
         ((0 589) (1 589) (2 589) (3 589) (4 589) (5 589) (6 589) (7 589)
          (8 589) (9 589)))))
      (589
       (Node
        (next_nodes
         ((0 590) (1 590) (2 590) (3 590) (4 590) (5 590) (6 590) (7 590)
          (8 590) (9 590)))))
      (590 (Node (next_nodes ((- 591)))))
      (591
       (Node
        (next_nodes
         ((0 592) (1 592) (2 592) (3 592) (4 592) (5 592) (6 592) (7 592)
          (8 592) (9 592)))))
      (592
       (Node
        (next_nodes
         ((0 593) (1 593) (2 593) (3 593) (4 593) (5 593) (6 593) (7 593)
          (8 593) (9 593)))))
      (593
       (Node
        (next_nodes
         ((0 594) (1 594) (2 594) (3 594) (4 594) (5 594) (6 594) (7 594)
          (8 594) (9 594)))))
      (594
       (Node
        (next_nodes
         ((0 595) (1 595) (2 595) (3 595) (4 595) (5 595) (6 595) (7 595)
          (8 595) (9 595)))))
      (595
       (Accept (next_nodes ())
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))
      (596
       (Node
        (next_nodes
         ((0 597) (1 597) (2 597) (3 597) (4 597) (5 597) (6 597) (7 597)
          (8 597) (9 597)))))
      (597
       (Node
        (next_nodes
         ((" " 598) (0 597) (1 597) (2 597) (3 597) (4 597) (5 597) (6 597)
          (7 597) (8 597) (9 597)))))
      (598
       (Node
        (next_nodes
         (("(" 582) (0 599) (1 599) (2 599) (3 599) (4 599) (5 599) (6 599)
          (7 599) (8 599) (9 599)))))
      (599
       (Node
        (next_nodes
         ((0 600) (1 600) (2 600) (3 600) (4 600) (5 600) (6 600) (7 600)
          (8 600) (9 600)))))
      (600
       (Node
        (next_nodes
         ((0 601) (1 601) (2 601) (3 601) (4 601) (5 601) (6 601) (7 601)
          (8 601) (9 601)))))
      (601
       (Node
        (next_nodes
         ((- 587) (0 602) (1 602) (2 602) (3 602) (4 602) (5 602) (6 602)
          (7 602) (8 602) (9 602)))))
      (602
       (Node
        (next_nodes
         ((0 603) (1 603) (2 603) (3 603) (4 603) (5 603) (6 603) (7 603)
          (8 603) (9 603)))))
      (603
       (Node
        (next_nodes
         ((0 591) (1 591) (2 591) (3 591) (4 591) (5 591) (6 591) (7 591)
          (8 591) (9 591)))))))
    |}]
;;
