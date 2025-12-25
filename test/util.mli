open! Core
open Import

module Common_config : sig
  val identifier : Regex_config.t
  val float : Regex_config.t
  val phone : Regex_config.t
end

module Common_lexer : sig
  module Programming_language : sig
    module Token : sig
      type t [@@deriving sexp_of]
    end

    include Lexer.S with type token := Token.t
  end
end
