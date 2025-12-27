open! Core

module Within_file : sig
  type t =
    { line_number : int
    ; column_number : int
    }
  [@@deriving equal, fields ~getters, sexp_of]
end

type t =
  { filename : string
  ; within_file : Within_file.t
  }
[@@deriving equal, fields ~getters, sexp_of]

module With_section : sig
  type 'a t =
    { value : 'a
    ; filename : string
    ; start : Within_file.t
    ; end_ : Within_file.t
    }
  [@@deriving equal, fields ~getters, sexp_of]
end
