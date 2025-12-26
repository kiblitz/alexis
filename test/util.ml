open! Core
open Import

module Common_config = struct
  let float =
    let any_digit = Regex_config.char_or (List.filter Char.all ~f:Char.is_digit) in
    let pre_decimal = Regex_config.plus any_digit in
    let decimal = Regex_config.Char '.' in
    let post_decimal = Regex_config.Star any_digit in
    Regex_config.concat [ pre_decimal; decimal; post_decimal ]
  ;;

  let identifier =
    let alphanum_or_underscore =
      Regex_config.char_or
        (List.filter Char.all ~f:(fun c -> Char.is_alphanum c || Char.equal c '_'))
    in
    let alpha_or_underscore =
      Regex_config.char_or
        (List.filter Char.all ~f:(fun c -> Char.is_alpha c || Char.equal c '_'))
    in
    Regex_config.concat [ alpha_or_underscore; Star alphanum_or_underscore ]
  ;;

  let phone =
    let any_digit = Regex_config.char_or (List.filter Char.all ~f:Char.is_digit) in
    let digit_group ?(count = 3) () =
      List.init count ~f:(Fn.const any_digit) |> Regex_config.concat
    in
    let country_code =
      let num = Regex_config.plus any_digit in
      let with_plus =
        Regex_config.concat [ Regex_config.Char '+'; num; Regex_config.Char ' ' ]
      in
      Regex_config.opt with_plus
    in
    let body_mod_country_code =
      let group3 = digit_group () in
      let group4 = digit_group ~count:4 () in
      let straight_mod_country_code = Regex_config.concat [ group3; group3; group4 ] in
      let dash_mod_country_code =
        Regex_config.concat
          [ group3; Regex_config.Char '-'; group3; Regex_config.Char '-'; group4 ]
      in
      let paren_mod_country_code =
        Regex_config.concat
          [ Regex_config.Char '('
          ; group3
          ; Regex_config.exact ") "
          ; group3
          ; Regex_config.Char '-'
          ; group4
          ]
      in
      Regex_config.or_
        [ straight_mod_country_code; dash_mod_country_code; paren_mod_country_code ]
    in
    Regex_config.concat [ country_code; body_mod_country_code ]
  ;;
end
