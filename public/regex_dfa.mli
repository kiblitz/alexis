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
end

type 'a t

val create : ?priority:int -> Config.t -> cont_of_match:(Buffer.t -> 'a) -> 'a t
val merge_list : 'a t Nonempty_list.t -> 'a t
