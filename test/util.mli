open! Core
open Import

module Common_config : sig
  val identifier : Regex_config.t
  val float : Regex_config.t
  val phone : Regex_config.t
end
