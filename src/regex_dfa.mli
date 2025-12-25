open! Core

module Config : sig
  type t =
    | Empty
    | Epsilon
    | Char of char
    | Concat of t * t
    | Or of t * t
    | Star of t

  val plus : t -> t
  val opt : t -> t
  val exact : string -> t
  val char_or : char list -> t
  val concat : t list -> t
  val or_ : t list -> t

  module For_testing : sig
    val sexp_of_t : t -> Sexp.t
    val reduce : t -> t
    val ( mod ) : t -> char -> t
    val possible_chars : t -> Char.Set.t
  end
end

type 'a t

val create : ?priority:int -> Config.t -> cont_of_match:(Buffer.t -> 'a) -> 'a t
val merge_list : 'a t Nonempty_list.t -> 'a t

module Iterator : sig
  type 'a node := 'a t
  type 'a t

  val make : ?initial_input_buffer_size:int -> 'a node -> 'a t

  module Result : sig
    type 'a t =
      | Incomplete of { is_accepting_state : bool }
      | Complete of
          { result : 'a
          ; unused_len : int
          }
      | Failure of { input : string }
    [@@deriving sexp_of]
  end

  val next : 'a t -> c:char -> 'a Result.t
end

module For_testing : sig
  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
end
