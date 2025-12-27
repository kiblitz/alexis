open! Core
open Import

module Token : sig
  type t [@@deriving sexp_of]
end

include Lexer.S with type token = Token.t
