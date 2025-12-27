include struct
  open Alexis_lib
  module Lexer = Lexer

  include struct
    open Source_position
    module With_section = With_section
  end

  module With_errors = With_errors
end
