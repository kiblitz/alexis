include struct
  open Alexis_lib
  module Regex_config = Regex_dfa.Config
  module Regex_dfa = Regex_dfa
  module Source_position = Source_position
  module With_errors = With_errors
end
