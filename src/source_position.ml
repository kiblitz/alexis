open! Core

module Within_file = struct
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

module With_section = struct
  type 'a t =
    { value : 'a
    ; filename : string
    ; start : Within_file.t
    ; end_ : Within_file.t
    }
  [@@deriving equal, fields ~getters]

  let sexp_of_t sexp_of_a { value; filename; start; end_ } =
    let module For_sexp_of = struct
      type 'a t =
        { value : 'a
        ; section : string
        }
      [@@deriving sexp_of]

      let create ~value ~filename ~(start : Within_file.t) ~(end_ : Within_file.t) =
        let section =
          [%string
            "%{filename}:%{start.line_number#Int},%{start.column_number#Int}-%{end_.line_number#Int},%{end_.column_number#Int}"]
        in
        { value; section }
      ;;
    end
    in
    For_sexp_of.sexp_of_t sexp_of_a (For_sexp_of.create ~value ~filename ~start ~end_)
  ;;
end
