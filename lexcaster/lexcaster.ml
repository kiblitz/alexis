open! Core
open! Import

let command ?docs (module M : Lexer.S) ~subcommand =
  let info =
    Cmdliner.Cmd.info ?docs ~doc:[%string "lexcaster for %{subcommand}"] subcommand
  in
  let main () =
    let lex input =
      let result = M.lex ~input ~filename:"lexcaster" in
      print_s
        [%message
          (result
           : M.token Alexis_lib.Source_position.With_section.t list
               Alexis_lib.With_errors.t)]
    in
    let rec loop () =
      print_string ">>> ";
      Out_channel.flush stdout;
      In_channel.input_line In_channel.stdin |> Option.iter ~f:lex;
      loop ()
    in
    loop ()
  in
  Cmdliner.Cmd.v info Cmdliner.Term.(const main $ const ())
;;
