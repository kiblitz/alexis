open! Core
open Import

module Token : sig
  module Identifier : sig
    type t = { name : string } [@@deriving equal, sexp_of]
  end

  module Big_identifier : sig
    type t = { name : string } [@@deriving equal, sexp_of]
  end

  module With_transformation : sig
    type t = Identifier.t option [@@deriving equal, sexp_of]
  end

  module Constant : sig
    type t =
      | Bool of bool
      | Int of string
      | Float of string
      | Char of string
      | String of string
    [@@deriving equal, sexp_of]
  end

  module Symbol : sig
    module Operator : sig
      module Base : sig
        type t =
          | Dot
          | Equal
          | Tilda
          | At
          | Caret
          | Pipe
          | Ampersand
          | Plus
          | Minus
          | Times
          | Div
          | Dollar
          | Percent
          | Greater
          | Less
        [@@deriving equal, sexp_of]
      end

      module Non_custom : sig
        type t = Double_colon [@@deriving equal, sexp_of]
      end

      type t =
        | Base of Base.t Nonempty_list.t
        | Non_custom of Non_custom.t
      [@@deriving equal, sexp_of]
    end

    type t =
      | Operator of Operator.t
      | Bang
      | Semicolon
      | Walrus
      | Colon
      | Comma
    [@@deriving equal, sexp_of]
  end

  module Import : sig
    type t =
      | Open of { allow_unused : bool }
      | Include
    [@@deriving equal, sexp_of]
  end

  module Definition : sig
    type t =
      | Assign_with_transformation of Identifier.t
      | Rec
      | And
      | In
      | Module of With_transformation.t
      | Sig
      | Struct
      | End
      | Underscore
    [@@deriving equal, sexp_of]
  end

  module Conditional : sig
    type t =
      | If of With_transformation.t
      | Then
      | Else
      | Match of With_transformation.t
      | When
      | Function
    [@@deriving equal, sexp_of]
  end

  module Typeful : sig
    type t =
      | Type
      | As
      | Of
      | Mutable
      | Nonrec
    [@@deriving equal, sexp_of]
  end

  module Left_or_right : sig
    type t =
      | Left
      | Right
    [@@deriving equal, sexp_of]
  end

  module Grouping : sig
    type t =
      | Parenthesis of Left_or_right.t
      | Curly_bracket of Left_or_right.t
      | Square_bracket of Left_or_right.t
    [@@deriving equal, sexp_of]
  end

  type t =
    | Constant of Constant.t
    | Identifier of Identifier.t
    | Big_identifier of Big_identifier.t
    | Symbol of Symbol.t
    | Definition of Definition.t
    | Conditional of Conditional.t
    | Typeful of Typeful.t
    | Grouping of Grouping.t
    | Import of Import.t
    | Lambda
    | Op
    | Functor
    | With
  [@@deriving equal, sexp_of]
end

include Lexer.S with type token = Token.t
