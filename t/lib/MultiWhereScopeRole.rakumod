role MWSRole[$type] { method type { $type } }
role MWSPlain { }
class MWSParser {
    multi method parse(Int $x, :$d) { "int" }
    multi method parse($x where MWSRole, :$d) { "role" }
    multi method parse($x where MWSPlain, :$d) { "plain" }
}
