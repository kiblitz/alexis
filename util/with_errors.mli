(* TODO: This should be upstreamed *)
open! Core

type 'a t =
  { value : 'a
  ; errors : Error.t list
  }
[@@deriving equal, fields ~getters, sexp_of]

include Monad.S with type 'a t := 'a t

val create_error : 'a -> Error.t -> 'a t
val error_s : 'a -> Sexp.t -> 'a t
