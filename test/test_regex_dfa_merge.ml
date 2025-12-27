open! Core
open! Import
open! Regex_dfa.For_testing

let merged_dfa =
  let to_dfa ~tag =
    let cont_of_match buffer = [%string "%{tag}:%{(Buffer.contents buffer)}"] in
    Regex_dfa.create ~cont_of_match
  in
  let in_ = Regex_config.exact "in" |> to_dfa ~tag:"in" in
  let int = Regex_config.exact "int" |> to_dfa ~tag:"int" in
  let id = to_dfa ~priority:0 Util.Common_config.identifier ~tag:"id" in
  Regex_dfa.merge_list Nonempty_list.[ in_; int; id ]
;;

let print_next_result ?(silent = false) iterator c =
  let result = Regex_dfa.Iterator.next iterator ~c:(Some c) in
  if not silent then print_s [%message (result : string Regex_dfa.Iterator.Result.t)]
;;

let%expect_test "merged dfa" =
  print_s [%message (merged_dfa : string t)];
  [%expect
    {|
    (merged_dfa
     ((563
       (Accept
        (next_nodes
         ((0 563) (1 563) (2 563) (3 563) (4 563) (5 563) (6 563) (7 563)
          (8 563) (9 563) (A 563) (B 563) (C 563) (D 563) (E 563) (F 563)
          (G 563) (H 563) (I 563) (J 563) (K 563) (L 563) (M 563) (N 563)
          (O 563) (P 563) (Q 563) (R 563) (S 563) (T 563) (U 563) (V 563)
          (W 563) (X 563) (Y 563) (Z 563) (_ 563) (a 563) (b 563) (c 563)
          (d 563) (e 563) (f 563) (g 563) (h 563) (i 563) (j 563) (k 563)
          (l 563) (m 563) (n 563) (o 563) (p 563) (q 563) (r 563) (s 563)
          (t 563) (u 563) (v 563) (w 563) (x 563) (y 563) (z 563)))
        (accepting_state_metadata ((priority 0) (cont_of_match <fun>)))))
      (567
       (Node
        (next_nodes
         ((A 563) (B 563) (C 563) (D 563) (E 563) (F 563) (G 563) (H 563)
          (I 563) (J 563) (K 563) (L 563) (M 563) (N 563) (O 563) (P 563)
          (Q 563) (R 563) (S 563) (T 563) (U 563) (V 563) (W 563) (X 563)
          (Y 563) (Z 563) (_ 563) (a 563) (b 563) (c 563) (d 563) (e 563)
          (f 563) (g 563) (h 563) (i 568) (j 563) (k 563) (l 563) (m 563)
          (n 563) (o 563) (p 563) (q 563) (r 563) (s 563) (t 563) (u 563)
          (v 563) (w 563) (x 563) (y 563) (z 563)))))
      (568
       (Accept
        (next_nodes
         ((0 563) (1 563) (2 563) (3 563) (4 563) (5 563) (6 563) (7 563)
          (8 563) (9 563) (A 563) (B 563) (C 563) (D 563) (E 563) (F 563)
          (G 563) (H 563) (I 563) (J 563) (K 563) (L 563) (M 563) (N 563)
          (O 563) (P 563) (Q 563) (R 563) (S 563) (T 563) (U 563) (V 563)
          (W 563) (X 563) (Y 563) (Z 563) (_ 563) (a 563) (b 563) (c 563)
          (d 563) (e 563) (f 563) (g 563) (h 563) (i 563) (j 563) (k 563)
          (l 563) (m 563) (n 569) (o 563) (p 563) (q 563) (r 563) (s 563)
          (t 563) (u 563) (v 563) (w 563) (x 563) (y 563) (z 563)))
        (accepting_state_metadata ((priority 0) (cont_of_match <fun>)))))
      (569
       (Accept
        (next_nodes
         ((0 563) (1 563) (2 563) (3 563) (4 563) (5 563) (6 563) (7 563)
          (8 563) (9 563) (A 563) (B 563) (C 563) (D 563) (E 563) (F 563)
          (G 563) (H 563) (I 563) (J 563) (K 563) (L 563) (M 563) (N 563)
          (O 563) (P 563) (Q 563) (R 563) (S 563) (T 563) (U 563) (V 563)
          (W 563) (X 563) (Y 563) (Z 563) (_ 563) (a 563) (b 563) (c 563)
          (d 563) (e 563) (f 563) (g 563) (h 563) (i 563) (j 563) (k 563)
          (l 563) (m 563) (n 563) (o 563) (p 563) (q 563) (r 563) (s 563)
          (t 570) (u 563) (v 563) (w 563) (x 563) (y 563) (z 563)))
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))
      (570
       (Accept
        (next_nodes
         ((0 563) (1 563) (2 563) (3 563) (4 563) (5 563) (6 563) (7 563)
          (8 563) (9 563) (A 563) (B 563) (C 563) (D 563) (E 563) (F 563)
          (G 563) (H 563) (I 563) (J 563) (K 563) (L 563) (M 563) (N 563)
          (O 563) (P 563) (Q 563) (R 563) (S 563) (T 563) (U 563) (V 563)
          (W 563) (X 563) (Y 563) (Z 563) (_ 563) (a 563) (b 563) (c 563)
          (d 563) (e 563) (f 563) (g 563) (h 563) (i 563) (j 563) (k 563)
          (l 563) (m 563) (n 563) (o 563) (p 563) (q 563) (r 563) (s 563)
          (t 563) (u 563) (v 563) (w 563) (x 563) (y 563) (z 563)))
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))))
    |}]
;;

let%expect_test "merged dfa iteration (success)" =
  let () =
    let id_iterator = Regex_dfa.Iterator.make merged_dfa in
    print_next_result id_iterator 'F';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result id_iterator 'o';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result id_iterator 'o';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result id_iterator '2';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result id_iterator '\n';
    [%expect {| (result (Complete (result id:Foo2) (unused_len 1))) |}]
  in
  let () =
    let in_iterator = Regex_dfa.Iterator.make merged_dfa in
    print_next_result in_iterator 'i';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result in_iterator 'n';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result in_iterator '\n';
    [%expect {| (result (Complete (result in:in) (unused_len 1))) |}]
  in
  let () =
    let int_iterator = Regex_dfa.Iterator.make merged_dfa in
    print_next_result int_iterator 'i';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result int_iterator 'n';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result int_iterator 't';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result int_iterator '\n';
    [%expect {| (result (Complete (result int:int) (unused_len 1))) |}]
  in
  ()
;;

let%expect_test "merged dfa iteration (failure)" =
  let iterator = Regex_dfa.Iterator.make merged_dfa in
  print_next_result iterator '3';
  [%expect {| (result (Failure (input 3))) |}]
;;
