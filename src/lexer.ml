open! Core

module type S = Lexer_intf.Lexer_s

module To_process = struct
  module Current_lex_token_position_info = struct
    type t =
      { start : Source_position.Within_file.t
      ; current : Source_position.Within_file.t
      }
    [@@deriving sexp_of]

    let empty =
      { start = { line_number = 0; column_number = 0 }
      ; current = { line_number = 0; column_number = 0 }
      }
    ;;

    let create source_position = { start = source_position; current = source_position }
  end

  type t =
    { input : string
    ; filename : string
    ; current_index : int
    ; current_lex_token_position_info : Current_lex_token_position_info.t
    ; last_accepting_state_index_and_position :
        (* TODO: this should be a labelled tuple *)
        (int * Source_position.Within_file.t) option
    }
  [@@deriving sexp_of]

  let create input ~filename =
    { input
    ; filename
    ; current_index = 0
    ; current_lex_token_position_info = Current_lex_token_position_info.empty
    ; last_accepting_state_index_and_position = None
    }
  ;;

  let next
        ?(reset_current_lext_token_position_info = false)
        ({ input
         ; filename = _
         ; current_index
         ; current_lex_token_position_info
         ; last_accepting_state_index_and_position = _
         } as t)
    =
    let { Source_position.Within_file.line_number; column_number } =
      current_lex_token_position_info.current
    in
    if current_index < String.length input
    then (
      let char = input.[current_index] in
      let (current : Source_position.Within_file.t) =
        match char with
        | '\n' -> { line_number = line_number + 1; column_number = 0 }
        | (_ : char) -> { line_number; column_number = column_number + 1 }
      in
      Some
        ( { t with
            current_index = current_index + 1
          ; current_lex_token_position_info =
              { start =
                  (if reset_current_lext_token_position_info
                   then current
                   else current_lex_token_position_info.start)
              ; current
              }
          }
        , char ))
    else None
  ;;

  let update_last_accepting_state t =
    { t with
      last_accepting_state_index_and_position =
        Some (t.current_index, t.current_lex_token_position_info.current)
    }
  ;;

  let result_and_reset t ~result =
    let%map.Or_error current_index, source_position =
      Or_error.of_option
        t.last_accepting_state_index_and_position
        ~error:
          (Error.create_s
             [%message "Applying last accepting state when none exists" (t : t)])
    in
    let result =
      { Source_position.With_section.value = result
      ; filename = t.filename
      ; start = t.current_lex_token_position_info.start
      ; end_ = source_position
      }
    in
    (* TODO this should be a labelled tuple *)
    ( result
    , { t with
        current_index
      ; current_lex_token_position_info =
          Current_lex_token_position_info.create source_position
      ; last_accepting_state_index_and_position = None
      } )
  ;;
end

module Make (M : Lexer_intf.Config_s) = struct
  include M

  let lex ~input ~filename =
    let rec loop_whitestring to_process =
      (let%bind.Option to_process = to_process in
       let%map.Option next_to_process, c =
         To_process.next ~reset_current_lext_token_position_info:true to_process
       in
       if Char.is_whitespace c
       then loop_whitestring (Some next_to_process)
       else on_non_whitespace to_process)
      |> Option.value ~default:(With_errors.return [])
    and on_non_whitespace to_process' =
      let iterator = Regex_dfa.Iterator.make M.token_dfa in
      let rec loop to_process =
        let next_to_process, c =
          match To_process.next to_process with
          | Some (next_to_process, c) -> Some next_to_process, Some c
          | None -> None, None
        in
        let latest_to_process = Option.value next_to_process ~default:to_process in
        match Regex_dfa.Iterator.next iterator ~c with
        | Complete { result; unused_len = _ } ->
          (match To_process.result_and_reset latest_to_process ~result with
           | Error error -> With_errors.create_error [] error
           | Ok (result, next_to_process) ->
             let%map.With_errors next_results = loop_whitestring (Some next_to_process) in
             result :: next_results)
        | Failure { input } ->
          let section =
            let { To_process.Current_lex_token_position_info.start; current } =
              to_process.current_lex_token_position_info
            in
            { Source_position.With_section.value = input
            ; filename
            ; start
            ; end_ = current
            }
          in
          With_errors.error_s
            []
            [%message "Failed to lex" (section : string Source_position.With_section.t)]
        | Incomplete { is_accepting_state = false } -> loop latest_to_process
        | Incomplete { is_accepting_state = true } ->
          loop (To_process.update_last_accepting_state latest_to_process)
      in
      loop to_process'
    in
    loop_whitestring (To_process.create input ~filename |> Some)
  ;;
end
