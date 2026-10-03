# Uses the enum module; the enum's bare values are lexical to this file, but
# its package-qualified ones (`PET::Msg::Ping`) are global.
use PET::Msg::Opcode;
class PET::Msg { has $.opcode }
