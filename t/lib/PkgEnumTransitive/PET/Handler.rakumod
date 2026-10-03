# Never `use`s PET::Msg::Opcode itself; names its values package-qualified.
use PET::Msg;
class PET::Handler {
    method kind($m) {
        given $m.opcode {
            when PET::Msg::Ping { 'ping' }
            default { 'other' }
        }
    }
}
