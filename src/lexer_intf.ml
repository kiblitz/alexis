open! Core
open! Import

module type Config_s = sig
  type token [@@deriving equal, sexp_of]

  val token_dfa : token Regex_dfa.t
end

module type Lexer_s = sig
  type token [@@deriving equal, sexp_of]

  val lex
    :  input:string
    -> filename:string
    -> token Source_position.With_section.t list With_errors.t
end

module type S = sig
  module type S = Lexer_s

  module Make (M : Config_s) : S with type token := M.token
end
