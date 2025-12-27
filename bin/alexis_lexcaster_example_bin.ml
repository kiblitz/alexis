open! Core
open Alexis_examples
module Lexcaster = Alexis_lexcaster.Lexcaster

let query_lang =
  Lexcaster.command
    ~docs:"example query language (quit=[quit ()])"
    ~exit_token_sequence:
      Query_lang.Token.[ Column { id = "quit" }; Parenthesis Left; Parenthesis Right ]
    (module Query_lang)
    ~subcommand:"query-lang"
;;

let ml_lang =
  Lexcaster.command
    ~docs:"example ml-family programming language (quit=[quit ()])"
    ~exit_token_sequence:
      Ml_lang.Token.
        [ Identifier { name = "quit" }
        ; Grouping (Parenthesis Left)
        ; Grouping (Parenthesis Right)
        ]
    (module Ml_lang)
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
  Cmdliner.Cmd.group info [ query_lang; ml_lang ]
;;

let () = Cmdliner.Cmd.eval command |> exit
