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
     ((562
       (Accept
        (next_nodes
         ((0 562) (1 562) (2 562) (3 562) (4 562) (5 562) (6 562) (7 562)
          (8 562) (9 562) (A 562) (B 562) (C 562) (D 562) (E 562) (F 562)
          (G 562) (H 562) (I 562) (J 562) (K 562) (L 562) (M 562) (N 562)
          (O 562) (P 562) (Q 562) (R 562) (S 562) (T 562) (U 562) (V 562)
          (W 562) (X 562) (Y 562) (Z 562) (_ 562) (a 562) (b 562) (c 562)
          (d 562) (e 562) (f 562) (g 562) (h 562) (i 562) (j 562) (k 562)
          (l 562) (m 562) (n 562) (o 562) (p 562) (q 562) (r 562) (s 562)
          (t 562) (u 562) (v 562) (w 562) (x 562) (y 562) (z 562)))
        (accepting_state_metadata ((priority 0) (cont_of_match <fun>)))))
      (566
       (Node
        (next_nodes
         ((A 562) (B 562) (C 562) (D 562) (E 562) (F 562) (G 562) (H 562)
          (I 562) (J 562) (K 562) (L 562) (M 562) (N 562) (O 562) (P 562)
          (Q 562) (R 562) (S 562) (T 562) (U 562) (V 562) (W 562) (X 562)
          (Y 562) (Z 562) (_ 562) (a 562) (b 562) (c 562) (d 562) (e 562)
          (f 562) (g 562) (h 562) (i 567) (j 562) (k 562) (l 562) (m 562)
          (n 562) (o 562) (p 562) (q 562) (r 562) (s 562) (t 562) (u 562)
          (v 562) (w 562) (x 562) (y 562) (z 562)))))
      (567
       (Accept
        (next_nodes
         ((0 562) (1 562) (2 562) (3 562) (4 562) (5 562) (6 562) (7 562)
          (8 562) (9 562) (A 562) (B 562) (C 562) (D 562) (E 562) (F 562)
          (G 562) (H 562) (I 562) (J 562) (K 562) (L 562) (M 562) (N 562)
          (O 562) (P 562) (Q 562) (R 562) (S 562) (T 562) (U 562) (V 562)
          (W 562) (X 562) (Y 562) (Z 562) (_ 562) (a 562) (b 562) (c 562)
          (d 562) (e 562) (f 562) (g 562) (h 562) (i 562) (j 562) (k 562)
          (l 562) (m 562) (n 568) (o 562) (p 562) (q 562) (r 562) (s 562)
          (t 562) (u 562) (v 562) (w 562) (x 562) (y 562) (z 562)))
        (accepting_state_metadata ((priority 0) (cont_of_match <fun>)))))
      (568
       (Accept
        (next_nodes
         ((0 562) (1 562) (2 562) (3 562) (4 562) (5 562) (6 562) (7 562)
          (8 562) (9 562) (A 562) (B 562) (C 562) (D 562) (E 562) (F 562)
          (G 562) (H 562) (I 562) (J 562) (K 562) (L 562) (M 562) (N 562)
          (O 562) (P 562) (Q 562) (R 562) (S 562) (T 562) (U 562) (V 562)
          (W 562) (X 562) (Y 562) (Z 562) (_ 562) (a 562) (b 562) (c 562)
          (d 562) (e 562) (f 562) (g 562) (h 562) (i 562) (j 562) (k 562)
          (l 562) (m 562) (n 562) (o 562) (p 562) (q 562) (r 562) (s 562)
          (t 569) (u 562) (v 562) (w 562) (x 562) (y 562) (z 562)))
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))
      (569
       (Accept
        (next_nodes
         ((0 562) (1 562) (2 562) (3 562) (4 562) (5 562) (6 562) (7 562)
          (8 562) (9 562) (A 562) (B 562) (C 562) (D 562) (E 562) (F 562)
          (G 562) (H 562) (I 562) (J 562) (K 562) (L 562) (M 562) (N 562)
          (O 562) (P 562) (Q 562) (R 562) (S 562) (T 562) (U 562) (V 562)
          (W 562) (X 562) (Y 562) (Z 562) (_ 562) (a 562) (b 562) (c 562)
          (d 562) (e 562) (f 562) (g 562) (h 562) (i 562) (j 562) (k 562)
          (l 562) (m 562) (n 562) (o 562) (p 562) (q 562) (r 562) (s 562)
          (t 562) (u 562) (v 562) (w 562) (x 562) (y 562) (z 562)))
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
