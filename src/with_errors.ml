open! Core

module T = struct
  type 'a t =
    { value : 'a
    ; errors : Error.t list
    }
  [@@deriving sexp_of]

  let return value = { value; errors = [] }

  let bind { value; errors } ~f =
    let t = f value in
    { value = t.value; errors = errors @ t.errors }
  ;;

  let map = `Define_using_bind
end

include T
include Monad.Make (T)

let create_error value error = { value; errors = [ error ] }

let error_s value sexp =
  let error = Error.create_s sexp in
  create_error value error
;;
