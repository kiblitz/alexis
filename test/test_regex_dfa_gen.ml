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
     ((570 (Node (next_nodes ((f 571))))) (571 (Node (next_nodes ((o 572)))))
      (572 (Node (next_nodes ((o 573))))) (573 (Node (next_nodes ((" " 574)))))
      (574
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
     ((575 (Node (next_nodes ((a 576) (b 576) (r 576)))))
      (576
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
     ((577
       (Node
        (next_nodes
         ((0 578) (1 578) (2 578) (3 578) (4 578) (5 578) (6 578) (7 578)
          (8 578) (9 578)))))
      (578
       (Node
        (next_nodes
         ((. 579) (0 578) (1 578) (2 578) (3 578) (4 578) (5 578) (6 578)
          (7 578) (8 578) (9 578)))))
      (579
       (Accept
        (next_nodes
         ((0 579) (1 579) (2 579) (3 579) (4 579) (5 579) (6 579) (7 579)
          (8 579) (9 579)))
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))))
    |}]
;;

let%expect_test "complex dfa: phone" =
  let dfa = to_dfa Util.Common_config.phone in
  print_s [%message (dfa : unit t)];
  [%expect
    {|
    (dfa
     ((580
       (Node
        (next_nodes
         (("(" 581) (+ 595) (0 598) (1 598) (2 598) (3 598) (4 598) (5 598)
          (6 598) (7 598) (8 598) (9 598)))))
      (581
       (Node
        (next_nodes
         ((0 582) (1 582) (2 582) (3 582) (4 582) (5 582) (6 582) (7 582)
          (8 582) (9 582)))))
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
      (584 (Node (next_nodes ((")" 585))))) (585 (Node (next_nodes ((" " 586)))))
      (586
       (Node
        (next_nodes
         ((0 587) (1 587) (2 587) (3 587) (4 587) (5 587) (6 587) (7 587)
          (8 587) (9 587)))))
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
      (589 (Node (next_nodes ((- 590)))))
      (590
       (Node
        (next_nodes
         ((0 591) (1 591) (2 591) (3 591) (4 591) (5 591) (6 591) (7 591)
          (8 591) (9 591)))))
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
       (Accept (next_nodes ())
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))
      (595
       (Node
        (next_nodes
         ((0 596) (1 596) (2 596) (3 596) (4 596) (5 596) (6 596) (7 596)
          (8 596) (9 596)))))
      (596
       (Node
        (next_nodes
         ((" " 597) (0 596) (1 596) (2 596) (3 596) (4 596) (5 596) (6 596)
          (7 596) (8 596) (9 596)))))
      (597
       (Node
        (next_nodes
         (("(" 581) (0 598) (1 598) (2 598) (3 598) (4 598) (5 598) (6 598)
          (7 598) (8 598) (9 598)))))
      (598
       (Node
        (next_nodes
         ((0 599) (1 599) (2 599) (3 599) (4 599) (5 599) (6 599) (7 599)
          (8 599) (9 599)))))
      (599
       (Node
        (next_nodes
         ((0 600) (1 600) (2 600) (3 600) (4 600) (5 600) (6 600) (7 600)
          (8 600) (9 600)))))
      (600
       (Node
        (next_nodes
         ((- 586) (0 601) (1 601) (2 601) (3 601) (4 601) (5 601) (6 601)
          (7 601) (8 601) (9 601)))))
      (601
       (Node
        (next_nodes
         ((0 602) (1 602) (2 602) (3 602) (4 602) (5 602) (6 602) (7 602)
          (8 602) (9 602)))))
      (602
       (Node
        (next_nodes
         ((0 590) (1 590) (2 590) (3 590) (4 590) (5 590) (6 590) (7 590)
          (8 590) (9 590)))))))
    |}]
;;
