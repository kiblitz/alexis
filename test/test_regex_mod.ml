open! Core
open! Import
open Regex_config.For_testing

module Tester : sig
  type t

  val run_test : (t -> unit) -> unit
  val print : t -> unit
  val init : ?no_reduce:unit -> ?silent:unit -> t -> Regex_config.t -> unit

  val apply_mod
    :  ?no_reduce:unit
    -> ?silent:unit
    -> ?config:Regex_config.t
    -> t
    -> char
    -> unit

  val undo : ?times:int -> t -> unit
end = struct
  type t =
    { mutable prev_configs : Regex_config.t list
    ; mutable running_mod : string
    }

  let run_test f =
    let t = { prev_configs = []; running_mod = "" } in
    f t
  ;;

  let init ?no_reduce ?silent t config =
    let config =
      match no_reduce with
      | None -> config
      | Some () -> reduce config
    in
    t.prev_configs <- [ config ];
    t.running_mod <- "";
    match silent with
    | Some () -> ()
    | None -> print_s [%message (config : t)]
  ;;

  let print t =
    let running_mod = t.running_mod in
    let new_config = List.hd_exn t.prev_configs in
    print_s [%message (running_mod : string) (new_config : t)]
  ;;

  let apply_mod ?no_reduce ?silent ?config t c =
    let config =
      match config with
      | None -> List.hd_exn t.prev_configs
      | Some config ->
        t.running_mod <- "";
        config
    in
    let new_config =
      match no_reduce with
      | None -> reduce (config mod c)
      | Some () -> config mod c
    in
    let new_running_mod = t.running_mod ^ Char.to_string c in
    t.prev_configs <- new_config :: t.prev_configs;
    t.running_mod <- new_running_mod;
    match silent with
    | Some () -> ()
    | None -> print t
  ;;

  let undo ?(times = 1) t =
    t.prev_configs <- List.drop t.prev_configs times;
    t.running_mod <- String.drop_suffix t.running_mod times
  ;;
end

open Tester

let%expect_test "exact modding" =
  run_test (fun t ->
    let config = Regex_config.exact "foo " in
    init t config;
    [%expect
      {| (config (Concat (Char f) (Concat (Char o) (Concat (Char o) (Char " "))))) |}];
    apply_mod t 'f';
    [%expect
      {| ((running_mod f) (new_config (Concat (Char o) (Concat (Char o) (Char " "))))) |}];
    apply_mod t 'o';
    [%expect {| ((running_mod fo) (new_config (Concat (Char o) (Char " ")))) |}];
    apply_mod ~config t 'z';
    [%expect {| ((running_mod z) (new_config Empty)) |}])
;;

let%expect_test "or modding" =
  run_test (fun t ->
    let config = Regex_config.char_or [ 'b'; 'a'; 'r' ] in
    init t config;
    [%expect {| (config (Or (Char b) (Or (Char a) (Char r)))) |}];
    apply_mod t 'b';
    [%expect {| ((running_mod b) (new_config Epsilon)) |}];
    apply_mod t 'z';
    [%expect {| ((running_mod bz) (new_config Empty)) |}];
    apply_mod ~config t 'z';
    [%expect {| ((running_mod z) (new_config Empty)) |}])
;;

let%expect_test "complex modding: floats" =
  run_test (fun t ->
    let config = Util.Common_config.float in
    init t config;
    [%expect
      {|
    (config
     (Concat
      (Concat
       (Or (Char 0)
        (Or (Char 1)
         (Or (Char 2)
          (Or (Char 3)
           (Or (Char 4)
            (Or (Char 5) (Or (Char 6) (Or (Char 7) (Or (Char 8) (Char 9))))))))))
       (Star
        (Or (Char 0)
         (Or (Char 1)
          (Or (Char 2)
           (Or (Char 3)
            (Or (Char 4)
             (Or (Char 5) (Or (Char 6) (Or (Char 7) (Or (Char 8) (Char 9))))))))))))
      (Concat (Char .)
       (Star
        (Or (Char 0)
         (Or (Char 1)
          (Or (Char 2)
           (Or (Char 3)
            (Or (Char 4)
             (Or (Char 5) (Or (Char 6) (Or (Char 7) (Or (Char 8) (Char 9))))))))))))))
    |}];
    apply_mod t '3';
    [%expect
      {|
      ((running_mod 3)
       (new_config
        (Concat
         (Star
          (Or (Char 0)
           (Or (Char 1)
            (Or (Char 2)
             (Or (Char 3)
              (Or (Char 4)
               (Or (Char 5) (Or (Char 6) (Or (Char 7) (Or (Char 8) (Char 9)))))))))))
         (Concat (Char .)
          (Star
           (Or (Char 0)
            (Or (Char 1)
             (Or (Char 2)
              (Or (Char 3)
               (Or (Char 4)
                (Or (Char 5) (Or (Char 6) (Or (Char 7) (Or (Char 8) (Char 9)))))))))))))))
      |}];
    apply_mod t '.';
    [%expect
      {|
      ((running_mod 3.)
       (new_config
        (Star
         (Or (Char 0)
          (Or (Char 1)
           (Or (Char 2)
            (Or (Char 3)
             (Or (Char 4)
              (Or (Char 5) (Or (Char 6) (Or (Char 7) (Or (Char 8) (Char 9)))))))))))))
      |}];
    apply_mod t '1';
    [%expect
      {|
      ((running_mod 3.1)
       (new_config
        (Star
         (Or (Char 0)
          (Or (Char 1)
           (Or (Char 2)
            (Or (Char 3)
             (Or (Char 4)
              (Or (Char 5) (Or (Char 6) (Or (Char 7) (Or (Char 8) (Char 9)))))))))))))
      |}];
    apply_mod t '4';
    [%expect
      {|
      ((running_mod 3.14)
       (new_config
        (Star
         (Or (Char 0)
          (Or (Char 1)
           (Or (Char 2)
            (Or (Char 3)
             (Or (Char 4)
              (Or (Char 5) (Or (Char 6) (Or (Char 7) (Or (Char 8) (Char 9)))))))))))))
      |}];
    apply_mod t '.';
    [%expect {| ((running_mod 3.14.) (new_config Empty)) |}];
    apply_mod ~config t '.';
    [%expect {| ((running_mod .) (new_config Empty)) |}];
    apply_mod ~config t 'z';
    [%expect {| ((running_mod z) (new_config Empty)) |}])
;;

let%expect_test "complex modding: phone" =
  run_test (fun t ->
    let config = Util.Common_config.phone in
    let apply_cc_mod () =
      apply_mod ~silent:() t '+';
      apply_mod ~silent:() t '1';
      apply_mod ~silent:() t ' '
    in
    let apply_group1_mod () =
      apply_mod ~silent:() t '1';
      apply_mod ~silent:() t '2';
      apply_mod ~silent:() t '3'
    in
    let apply_group2_mod () =
      apply_mod ~silent:() t '4';
      apply_mod ~silent:() t '5';
      apply_mod ~silent:() t '6'
    in
    let apply_group3_mod () =
      apply_mod ~silent:() t '7';
      apply_mod ~silent:() t '8';
      apply_mod ~silent:() t '9';
      apply_mod ~silent:() t '0'
    in
    let apply_straight_mod () =
      apply_group1_mod ();
      apply_group2_mod ();
      apply_group3_mod ()
    in
    init ~silent:() t config;
    apply_cc_mod ();
    apply_straight_mod ();
    undo ~times:2 t;
    apply_mod t '9';
    [%expect
      {|
      ((running_mod "+1 123456789")
       (new_config
        (Or (Char 0)
         (Or (Char 1)
          (Or (Char 2)
           (Or (Char 3)
            (Or (Char 4)
             (Or (Char 5) (Or (Char 6) (Or (Char 7) (Or (Char 8) (Char 9))))))))))))
      |}];
    apply_mod t '0';
    [%expect {| ((running_mod "+1 1234567890") (new_config Epsilon)) |}];
    init ~silent:() t config;
    apply_straight_mod ();
    print t;
    [%expect {| ((running_mod 1234567890) (new_config Epsilon)) |}];
    let apply_dash_mod () =
      apply_group1_mod ();
      apply_mod ~silent:() t '-';
      apply_group2_mod ();
      apply_mod ~silent:() t '-';
      apply_group3_mod ()
    in
    init ~silent:() t config;
    apply_cc_mod ();
    apply_dash_mod ();
    print t;
    [%expect {| ((running_mod "+1 123-456-7890") (new_config Epsilon)) |}];
    init ~silent:() t config;
    apply_dash_mod ();
    print t;
    [%expect {| ((running_mod 123-456-7890) (new_config Epsilon)) |}];
    let apply_paren_mod () =
      apply_mod ~silent:() t '(';
      apply_group1_mod ();
      apply_mod ~silent:() t ')';
      apply_mod ~silent:() t ' ';
      apply_group2_mod ();
      apply_mod ~silent:() t '-';
      apply_group3_mod ()
    in
    init ~silent:() t config;
    apply_cc_mod ();
    apply_paren_mod ();
    print t;
    [%expect {| ((running_mod "+1 (123) 456-7890") (new_config Epsilon)) |}];
    init ~silent:() t config;
    apply_paren_mod ();
    print t;
    [%expect {| ((running_mod "(123) 456-7890") (new_config Epsilon)) |}];
    apply_mod ~config t 'z';
    [%expect {| ((running_mod z) (new_config Empty)) |}])
;;
