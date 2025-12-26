open! Core
open! Import

val command : ?docs:string -> (module Lexer.S) -> subcommand:string -> unit Cmdliner.Cmd.t
