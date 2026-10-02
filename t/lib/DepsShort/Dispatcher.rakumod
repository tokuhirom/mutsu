use DepsShort::LC;
use DepsShort::Item::Sto;

class DepsShort::Dispatcher {
    multi method lm(Sto) { "S" }
    multi method lm(New) { "N" }
    method go($l) { self.lm($l) }
}
