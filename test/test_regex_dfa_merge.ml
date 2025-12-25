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
  let result = Regex_dfa.Iterator.next iterator ~c in
  if not silent then print_s [%message (result : string Regex_dfa.Iterator.Result.t)]
;;

let%expect_test "merged dfa" =
  print_s [%message (merged_dfa : string t)];
  [%expect
    {|
    (merged_dfa
     ((414
       (Accept
        (next_nodes
         ((0 414) (1 414) (2 414) (3 414) (4 414) (5 414) (6 414) (7 414)
          (8 414) (9 414) (A 414) (B 414) (C 414) (D 414) (E 414) (F 414)
          (G 414) (H 414) (I 414) (J 414) (K 414) (L 414) (M 414) (N 414)
          (O 414) (P 414) (Q 414) (R 414) (S 414) (T 414) (U 414) (V 414)
          (W 414) (X 414) (Y 414) (Z 414) (_ 414) (a 414) (b 414) (c 414)
          (d 414) (e 414) (f 414) (g 414) (h 414) (i 414) (j 414) (k 414)
          (l 414) (m 414) (n 414) (o 414) (p 414) (q 414) (r 414) (s 414)
          (t 414) (u 414) (v 414) (w 414) (x 414) (y 414) (z 414)))
        (accepting_state_metadata ((priority 0) (cont_of_match <fun>)))))
      (418
       (Node
        (next_nodes
         ((A 414) (B 414) (C 414) (D 414) (E 414) (F 414) (G 414) (H 414)
          (I 414) (J 414) (K 414) (L 414) (M 414) (N 414) (O 414) (P 414)
          (Q 414) (R 414) (S 414) (T 414) (U 414) (V 414) (W 414) (X 414)
          (Y 414) (Z 414) (_ 414) (a 414) (b 414) (c 414) (d 414) (e 414)
          (f 414) (g 414) (h 414) (i 419) (j 414) (k 414) (l 414) (m 414)
          (n 414) (o 414) (p 414) (q 414) (r 414) (s 414) (t 414) (u 414)
          (v 414) (w 414) (x 414) (y 414) (z 414)))))
      (419
       (Accept
        (next_nodes
         ((0 414) (1 414) (2 414) (3 414) (4 414) (5 414) (6 414) (7 414)
          (8 414) (9 414) (A 414) (B 414) (C 414) (D 414) (E 414) (F 414)
          (G 414) (H 414) (I 414) (J 414) (K 414) (L 414) (M 414) (N 414)
          (O 414) (P 414) (Q 414) (R 414) (S 414) (T 414) (U 414) (V 414)
          (W 414) (X 414) (Y 414) (Z 414) (_ 414) (a 414) (b 414) (c 414)
          (d 414) (e 414) (f 414) (g 414) (h 414) (i 414) (j 414) (k 414)
          (l 414) (m 414) (n 420) (o 414) (p 414) (q 414) (r 414) (s 414)
          (t 414) (u 414) (v 414) (w 414) (x 414) (y 414) (z 414)))
        (accepting_state_metadata ((priority 0) (cont_of_match <fun>)))))
      (420
       (Accept
        (next_nodes
         ((0 414) (1 414) (2 414) (3 414) (4 414) (5 414) (6 414) (7 414)
          (8 414) (9 414) (A 414) (B 414) (C 414) (D 414) (E 414) (F 414)
          (G 414) (H 414) (I 414) (J 414) (K 414) (L 414) (M 414) (N 414)
          (O 414) (P 414) (Q 414) (R 414) (S 414) (T 414) (U 414) (V 414)
          (W 414) (X 414) (Y 414) (Z 414) (_ 414) (a 414) (b 414) (c 414)
          (d 414) (e 414) (f 414) (g 414) (h 414) (i 414) (j 414) (k 414)
          (l 414) (m 414) (n 414) (o 414) (p 414) (q 414) (r 414) (s 414)
          (t 421) (u 414) (v 414) (w 414) (x 414) (y 414) (z 414)))
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))
      (421
       (Accept
        (next_nodes
         ((0 414) (1 414) (2 414) (3 414) (4 414) (5 414) (6 414) (7 414)
          (8 414) (9 414) (A 414) (B 414) (C 414) (D 414) (E 414) (F 414)
          (G 414) (H 414) (I 414) (J 414) (K 414) (L 414) (M 414) (N 414)
          (O 414) (P 414) (Q 414) (R 414) (S 414) (T 414) (U 414) (V 414)
          (W 414) (X 414) (Y 414) (Z 414) (_ 414) (a 414) (b 414) (c 414)
          (d 414) (e 414) (f 414) (g 414) (h 414) (i 414) (j 414) (k 414)
          (l 414) (m 414) (n 414) (o 414) (p 414) (q 414) (r 414) (s 414)
          (t 414) (u 414) (v 414) (w 414) (x 414) (y 414) (z 414)))
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
