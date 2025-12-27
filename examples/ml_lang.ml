open! Core
open! Import

module Identifier = struct
  type t = { name : string } [@@deriving sexp_of]
end

module Big_identifier = struct
  type t = { name : string } [@@deriving sexp_of]
end

module With_transformation = struct
  type t = Identifier.t option [@@deriving sexp_of]
end

module Constant = struct
  type t =
    | Bool of bool
    | Int of string
    | Float of string
    | Char of string
    | String of string
  [@@deriving sexp_of]
end

module Symbol = struct
  module Operator = struct
    module Base = struct
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
      [@@deriving sexp_of]
    end

    module Non_custom = struct
      type t = Double_colon [@@deriving sexp_of]
    end

    type t =
      | Base of Base.t Nonempty_list.t
      | Non_custom of Non_custom.t
    [@@deriving sexp_of]
  end

  type t =
    | Operator of Operator.t
    | Bang
    | Semicolon
    | Walrus
    | Colon
    | Comma
  [@@deriving sexp_of]
end

module Import = struct
  type t =
    | Open of { allow_unused : bool }
    | Include
  [@@deriving sexp_of]
end

module Definition = struct
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
  [@@deriving sexp_of]
end

module Conditional = struct
  type t =
    | If of With_transformation.t
    | Then
    | Else
    | Match of With_transformation.t
    | When
    | Function
  [@@deriving sexp_of]
end

module Typeful = struct
  type t =
    | Type
    | As
    | Of
    | Mutable
    | Nonrec
  [@@deriving sexp_of]
end

module Left_or_right = struct
  type t =
    | Left
    | Right
  [@@deriving sexp_of]
end

module Grouping = struct
  type t =
    | Parenthesis of Left_or_right.t
    | Curly_bracket of Left_or_right.t
    | Square_bracket of Left_or_right.t
  [@@deriving sexp_of]
end

module Token = struct
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
  [@@deriving sexp_of]
end

module Config = struct
  type token = Token.t [@@deriving sexp_of]

  let token_dfa =
    let open Regex_dfa.Config in
    let num =
      let char_or_digit = List.filter Char.all ~f:Char.is_digit in
      char_or char_or_digit
    in
    let single_char ~without =
      let backslash = '\\' in
      let escaped = Concat (Char backslash, char_or Char.all) in
      let char_or_unescaped =
        List.filter Char.all ~f:(fun c ->
          not (List.exists (backslash :: without) ~f:(Char.equal c)))
      in
      Or (escaped, char_or char_or_unescaped)
    in
    let identifier, big_identifier =
      let alphanum_or_underscore =
        char_or
          (List.filter Char.all ~f:(fun c -> Char.is_alphanum c || Char.equal c '_'))
      in
      let is_uppercase_alpha c = Char.is_alpha c && Char.is_uppercase c in
      let is_lowercase_alpha c = Char.is_alpha c && Char.is_lowercase c in
      let lower_alpha_or_underscore =
        char_or
          (List.filter Char.all ~f:(fun c -> is_lowercase_alpha c || Char.equal c '_'))
      in
      let upper_alpha_or_underscore =
        char_or
          (List.filter Char.all ~f:(fun c -> is_uppercase_alpha c || Char.equal c '_'))
      in
      ( concat [ lower_alpha_or_underscore; Star alphanum_or_underscore ]
      , concat [ upper_alpha_or_underscore; Star alphanum_or_underscore ] )
    in
    let with_transformation ?keyword () =
      let prefix =
        match keyword with
        | Some keyword -> keyword ^ "#"
        | None -> "#"
      in
      concat [ exact prefix; identifier ]
    in
    let buffer_suffix buffer ~pos =
      Buffer.sub buffer ~pos ~len:(Buffer.length buffer - pos)
    in
    let const_dfa config token =
      Regex_dfa.create config ~cont_of_match:(Fn.const token)
    in
    let text_dfa content ~constructor ~delimiter =
      Regex_dfa.create
        (concat [ Char delimiter; content; Char delimiter ])
        ~cont_of_match:(fun buffer ->
          let bytes = Buffer.sub buffer ~pos:1 ~len:(Buffer.length buffer - 2) in
          constructor (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes))
    in
    let with_transformation_dfa ?keyword () ~constructor =
      let keyword_len =
        Option.value_map keyword ~default:1 ~f:(fun keyword -> String.length keyword + 1)
      in
      Regex_dfa.create (with_transformation ?keyword ()) ~cont_of_match:(fun buffer ->
        let name =
          Bytes.unsafe_to_string
            ~no_mutation_while_string_reachable:(buffer_suffix buffer ~pos:keyword_len)
        in
        constructor { Identifier.name })
    in
    [ (* Constant *)
      const_dfa (exact "true") (Token.Constant (Bool true))
    ; const_dfa (exact "false") (Token.Constant (Bool false))
    ; Regex_dfa.create (plus num) ~cont_of_match:(fun buffer ->
        Token.Constant (Int (Buffer.contents buffer)))
    ; Regex_dfa.create
        (Or
           ( concat [ plus num; Char '.'; Star num ]
           , concat [ Star num; Char '.'; plus num ] ))
        ~cont_of_match:(fun buffer -> Token.Constant (Float (Buffer.contents buffer)))
    ; text_dfa
        (single_char ~without:[ '\'' ])
        ~constructor:(fun str -> Token.Constant (Char str))
        ~delimiter:'\''
    ; text_dfa
        (Star (single_char ~without:[ '\"' ]))
        ~constructor:(fun str -> Token.Constant (String str))
        ~delimiter:'\"'
    ; (* Identifier *)
      Regex_dfa.create ~priority:0 identifier ~cont_of_match:(fun buffer ->
        let name = Buffer.contents buffer in
        Token.Identifier { name })
    ; (* Big_identifier *)
      Regex_dfa.create ~priority:0 big_identifier ~cont_of_match:(fun buffer ->
        let name = Buffer.contents buffer in
        Token.Big_identifier { name })
    ; (* Symbol *)
      const_dfa (exact "::") (Token.Symbol (Operator (Non_custom Double_colon)))
    ; const_dfa (exact ",") (Token.Symbol Comma)
    ; const_dfa (exact ":") (Token.Symbol Colon)
    ; const_dfa (exact "!") (Token.Symbol Bang)
    ; const_dfa (exact ";") (Token.Symbol Semicolon)
    ; const_dfa (exact ":=") (Token.Symbol Walrus)
    ; Regex_dfa.create
        (plus
           (char_or
              [ '.'
              ; '='
              ; '~'
              ; '@'
              ; '^'
              ; '|'
              ; '&'
              ; '+'
              ; '-'
              ; '*'
              ; '/'
              ; '$'
              ; '%'
              ; '>'
              ; '<'
              ]))
        ~cont_of_match:(fun buffer ->
          let base_symbols =
            Buffer.contents buffer
            |> String.to_list
            |> Nonempty_list.of_list_exn
            |> Nonempty_list.map ~f:(function
              | '.' -> Symbol.Operator.Base.Dot
              | '=' -> Equal
              | '~' -> Tilda
              | '@' -> At
              | '^' -> Caret
              | '|' -> Pipe
              | '&' -> Ampersand
              | '+' -> Plus
              | '-' -> Minus
              | '*' -> Times
              | '/' -> Div
              | '$' -> Dollar
              | '%' -> Percent
              | '>' -> Greater
              | '<' -> Less
              | c -> raise_s [%message "Unexpected base symbol" (c : char)])
          in
          Token.Symbol (Operator (Base base_symbols)))
    ; (* Definition *)
      with_transformation_dfa () ~constructor:(fun id ->
        Token.Definition (Assign_with_transformation id))
    ; const_dfa (exact "rec") (Token.Definition Rec)
    ; const_dfa (exact "and") (Token.Definition And)
    ; const_dfa (exact "in") (Token.Definition In)
    ; const_dfa (exact "module") (Token.Definition (Module None))
    ; with_transformation_dfa ~keyword:"module" () ~constructor:(fun id ->
        Token.Definition (Module (Some id)))
    ; const_dfa (exact "sig") (Token.Definition Sig)
    ; const_dfa (exact "struct") (Token.Definition Struct)
    ; const_dfa (exact "end") (Token.Definition End)
    ; const_dfa (exact "_") (Token.Definition Underscore)
    ; (* Conditional*)
      const_dfa (exact "if") (Token.Conditional (If None))
    ; with_transformation_dfa ~keyword:"if" () ~constructor:(fun id ->
        Token.Conditional (If (Some id)))
    ; const_dfa (exact "then") (Token.Conditional Then)
    ; const_dfa (exact "else") (Token.Conditional Else)
    ; const_dfa (exact "match") (Token.Conditional (Match None))
    ; with_transformation_dfa ~keyword:"match" () ~constructor:(fun id ->
        Token.Conditional (Match (Some id)))
    ; const_dfa (exact "when") (Token.Conditional When)
    ; const_dfa (exact "function") (Token.Conditional Function)
    ; (* Typeful *)
      const_dfa (exact "type") (Token.Typeful Type)
    ; const_dfa (exact "as") (Token.Typeful As)
    ; const_dfa (exact "of") (Token.Typeful Of)
    ; const_dfa (exact "mutable") (Token.Typeful Mutable)
    ; const_dfa (exact "nonrec") (Token.Typeful Nonrec)
    ; (* Typeful *)
      const_dfa (exact "(") (Token.Grouping (Parenthesis Left))
    ; const_dfa (exact ")") (Token.Grouping (Parenthesis Right))
    ; const_dfa (exact "{") (Token.Grouping (Curly_bracket Left))
    ; const_dfa (exact "}") (Token.Grouping (Curly_bracket Right))
    ; const_dfa (exact "[") (Token.Grouping (Square_bracket Left))
    ; const_dfa (exact "]") (Token.Grouping (Square_bracket Right))
    ; (* Import *)
      const_dfa (exact "open") (Token.Import (Open { allow_unused = false }))
    ; const_dfa (exact "open!") (Token.Import (Open { allow_unused = true }))
    ; const_dfa (exact "include") (Token.Import Include)
    ; (* Misc *)
      const_dfa (exact "lambda") Token.Lambda
    ; const_dfa (exact "op") Token.Op
    ; const_dfa (exact "functor") Token.Functor
    ; const_dfa (exact "with") Token.With
    ]
    |> Regex_dfa.merge_list
  ;;
end

include Config
include Lexer.Make (Config)
