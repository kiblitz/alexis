open! Core
open Import

module Token : sig
  module Keyword : sig
    type t =
      | Select
      | Where
      | Like
    [@@deriving equal, sexp_of]
  end

  module Constant : sig
    type t =
      | Null
      | Bool of bool
      | Num of string
      | Date of string
      | String of string
    [@@deriving equal, sexp_of]
  end

  module Clause : sig
    type t =
      | Equal
      | Less_than
      | Less_than_or_equal
      | Greater_than
      | Greater_than_or_equal
      | Not_equal
      | And
      | Or
    [@@deriving equal, sexp_of]
  end

  module Parenthesis : sig
    type t =
      | Left
      | Right
    [@@deriving equal, sexp_of]
  end

  type t =
    | Keyword of Keyword.t
    | Constant of Constant.t
    | Column of { id : string }
    | Clause of Clause.t
    | Parenthesis of Parenthesis.t
    | Star
  [@@deriving equal, sexp_of]
end

include Lexer.S with type token = Token.t
