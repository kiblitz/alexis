open! Core
open! Import

val command
  :  ?docs:string
  -> ?exit_token_sequence:'token list
  -> (module Lexer.S with type token = 'token)
  -> subcommand:string
  -> unit Cmdliner.Cmd.t
