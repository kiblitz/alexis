open! Core
open! Import

module Keyword = struct
  type t =
    | Select
    | From
    | Where
    | Like
  [@@deriving sexp_of]
end

module Constant = struct
  type t =
    | Null
    | Bool of bool
    | Num of string
    | Date of string
    | String of string
  [@@deriving sexp_of]
end

module Clause = struct
  type t =
    | Equal
    | Less_than
    | Less_than_or_equal
    | Greater_than
    | Greater_than_or_equal
    | Not_equal
    | And
    | Or
  [@@deriving sexp_of]
end

module Parenthesis = struct
  type t =
    | Left
    | Right
  [@@deriving sexp_of]
end

module Token = struct
  type t =
    | Keyword of Keyword.t
    | Constant of Constant.t
    | Column of { id : string }
    | Clause of Clause.t
    | Parenthesis of Parenthesis.t
    | Star
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
    let column =
      let alphanum_or_underscore =
        char_or
          (List.filter Char.all ~f:(fun c -> Char.is_alphanum c || Char.equal c '_'))
      in
      let alpha = char_or (List.filter Char.all ~f:Char.is_alpha) in
      concat [ alpha; Star alphanum_or_underscore ]
    in
    let exact_case_insensitive s =
      String.to_list s
      |> List.map ~f:(fun c -> char_or [ Char.lowercase c; Char.uppercase c ])
      |> concat
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
    [ (* Keyword *)
      const_dfa (exact_case_insensitive "select") (Token.Keyword Select)
    ; const_dfa (exact_case_insensitive "from") (Token.Keyword From)
    ; const_dfa (exact_case_insensitive "where") (Token.Keyword Where)
    ; const_dfa (exact_case_insensitive "like") (Token.Keyword Like)
    ; (* Constant*)
      const_dfa (exact_case_insensitive "null") (Token.Constant Null)
    ; const_dfa (exact_case_insensitive "true") (Token.Constant (Bool true))
    ; const_dfa (exact_case_insensitive "false") (Token.Constant (Bool false))
    ; Regex_dfa.create
        (or_
           [ concat [ plus num; Char '.'; Star num ]
           ; concat [ Star num; Char '.'; plus num ]
           ; plus num
           ])
        ~cont_of_match:(fun buffer -> Token.Constant (Num (Buffer.contents buffer)))
    ; Regex_dfa.create
        (concat [ num; num; Char '-'; num; num; Char '-'; num; num; num; num ])
        ~cont_of_match:(fun buffer -> Token.Constant (Date (Buffer.contents buffer)))
    ; text_dfa
        (Star (single_char ~without:[ '\"' ]))
        ~constructor:(fun str -> Token.Constant (String str))
        ~delimiter:'\"'
    ; text_dfa
        (Star (single_char ~without:[ '\'' ]))
        ~constructor:(fun str -> Token.Constant (String str))
        ~delimiter:'\''
    ; (* Column *)
      Regex_dfa.create column ~cont_of_match:(fun buffer ->
        Token.Column { id = Buffer.contents buffer })
    ; (* Clause *)
      const_dfa (exact "=") (Token.Clause Equal)
    ; const_dfa (exact "<") (Token.Clause Less_than)
    ; const_dfa (exact "<=") (Token.Clause Less_than_or_equal)
    ; const_dfa (exact ">") (Token.Clause Greater_than)
    ; const_dfa (exact ">=") (Token.Clause Greater_than_or_equal)
    ; const_dfa (exact "<>") (Token.Clause Not_equal)
    ; const_dfa (exact_case_insensitive "and") (Token.Clause And)
    ; const_dfa (exact_case_insensitive "or") (Token.Clause Or)
    ; (* Parenthesis *)
      const_dfa (exact "(") (Token.Parenthesis Left)
    ; const_dfa (exact ")") (Token.Parenthesis Right)
    ; (* Misc *)
      const_dfa (exact "*") Token.Star
    ]
    |> Regex_dfa.merge_list
  ;;
end

include Config
include Lexer.Make (Config)
