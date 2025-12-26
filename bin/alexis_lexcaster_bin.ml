open! Core
module Lexcaster = Alexis_lexcaster.Lexcaster

let ml_lang =
  Lexcaster.command
    ~docs:"example ml-family programming language"
    (module Alexis_examples.Ml_lang)
    ~subcommand:"ml-lang"
;;

let command =
  let info =
    Cmdliner.Cmd.info
      ~docs:
        "this tool serves as an example lexcaster on a set of example lexers defined in \
         alexis/examples/"
      ~doc:"cli tool for evaluating alexis on user input"
      "alexis_lexcaster_example_bin"
  in
  Cmdliner.Cmd.group info [ ml_lang ]
;;

let () = Cmdliner.Cmd.eval command |> exit
