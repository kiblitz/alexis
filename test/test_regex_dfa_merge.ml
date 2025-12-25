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
     ((8
       (Accept
        (next_nodes
         ((0 8) (1 8) (2 8) (3 8) (4 8) (5 8) (6 8) (7 8) (8 8) (9 8) (A 8)
          (B 8) (C 8) (D 8) (E 8) (F 8) (G 8) (H 8) (I 8) (J 8) (K 8) (L 8)
          (M 8) (N 8) (O 8) (P 8) (Q 8) (R 8) (S 8) (T 8) (U 8) (V 8) (W 8)
          (X 8) (Y 8) (Z 8) (_ 8) (a 8) (b 8) (c 8) (d 8) (e 8) (f 8) (g 8)
          (h 8) (i 8) (j 8) (k 8) (l 8) (m 8) (n 8) (o 8) (p 8) (q 8) (r 8)
          (s 8) (t 8) (u 8) (v 8) (w 8) (x 8) (y 8) (z 8)))
        (accepting_state_metadata ((priority 0) (cont_of_match <fun>)))))
      (12
       (Node
        (next_nodes
         ((A 8) (B 8) (C 8) (D 8) (E 8) (F 8) (G 8) (H 8) (I 8) (J 8) (K 8)
          (L 8) (M 8) (N 8) (O 8) (P 8) (Q 8) (R 8) (S 8) (T 8) (U 8) (V 8)
          (W 8) (X 8) (Y 8) (Z 8) (_ 8) (a 8) (b 8) (c 8) (d 8) (e 8) (f 8)
          (g 8) (h 8) (i 13) (j 8) (k 8) (l 8) (m 8) (n 8) (o 8) (p 8) (q 8)
          (r 8) (s 8) (t 8) (u 8) (v 8) (w 8) (x 8) (y 8) (z 8)))))
      (13
       (Accept
        (next_nodes
         ((0 8) (1 8) (2 8) (3 8) (4 8) (5 8) (6 8) (7 8) (8 8) (9 8) (A 8)
          (B 8) (C 8) (D 8) (E 8) (F 8) (G 8) (H 8) (I 8) (J 8) (K 8) (L 8)
          (M 8) (N 8) (O 8) (P 8) (Q 8) (R 8) (S 8) (T 8) (U 8) (V 8) (W 8)
          (X 8) (Y 8) (Z 8) (_ 8) (a 8) (b 8) (c 8) (d 8) (e 8) (f 8) (g 8)
          (h 8) (i 8) (j 8) (k 8) (l 8) (m 8) (n 14) (o 8) (p 8) (q 8) (r 8)
          (s 8) (t 8) (u 8) (v 8) (w 8) (x 8) (y 8) (z 8)))
        (accepting_state_metadata ((priority 0) (cont_of_match <fun>)))))
      (14
       (Accept
        (next_nodes
         ((0 8) (1 8) (2 8) (3 8) (4 8) (5 8) (6 8) (7 8) (8 8) (9 8) (A 8)
          (B 8) (C 8) (D 8) (E 8) (F 8) (G 8) (H 8) (I 8) (J 8) (K 8) (L 8)
          (M 8) (N 8) (O 8) (P 8) (Q 8) (R 8) (S 8) (T 8) (U 8) (V 8) (W 8)
          (X 8) (Y 8) (Z 8) (_ 8) (a 8) (b 8) (c 8) (d 8) (e 8) (f 8) (g 8)
          (h 8) (i 8) (j 8) (k 8) (l 8) (m 8) (n 8) (o 8) (p 8) (q 8) (r 8)
          (s 8) (t 15) (u 8) (v 8) (w 8) (x 8) (y 8) (z 8)))
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))
      (15
       (Accept
        (next_nodes
         ((0 8) (1 8) (2 8) (3 8) (4 8) (5 8) (6 8) (7 8) (8 8) (9 8) (A 8)
          (B 8) (C 8) (D 8) (E 8) (F 8) (G 8) (H 8) (I 8) (J 8) (K 8) (L 8)
          (M 8) (N 8) (O 8) (P 8) (Q 8) (R 8) (S 8) (T 8) (U 8) (V 8) (W 8)
          (X 8) (Y 8) (Z 8) (_ 8) (a 8) (b 8) (c 8) (d 8) (e 8) (f 8) (g 8)
          (h 8) (i 8) (j 8) (k 8) (l 8) (m 8) (n 8) (o 8) (p 8) (q 8) (r 8)
          (s 8) (t 8) (u 8) (v 8) (w 8) (x 8) (y 8) (z 8)))
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
