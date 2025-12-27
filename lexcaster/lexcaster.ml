open! Core
open! Import

let command
      (type a)
      ?docs
      ?(exit_token_sequence : a list = [])
      (module M : Lexer.S with type token = a)
      ~subcommand
  =
  let info =
    Cmdliner.Cmd.info ?docs ~doc:[%string "lexcaster for %{subcommand}"] subcommand
  in
  let main () =
    let lex input =
      let result = M.lex ~input ~filename:"lexcaster" in
      print_s [%message (result : M.token With_section.t list With_errors.t)];
      if
        [%equal: M.token list]
          (result |> With_errors.value |> List.map ~f:With_section.value)
          exit_token_sequence
      then `Exit
      else `Continue
    in
    let rec loop () =
      print_string ">>> ";
      Out_channel.flush stdout;
      let result = In_channel.input_line In_channel.stdin |> Option.map ~f:lex in
      match result with
      | None | Some `Continue -> loop ()
      | Some `Exit -> ()
    in
    loop ()
  in
  Cmdliner.Cmd.v info Cmdliner.Term.(const main $ const ())
;;
