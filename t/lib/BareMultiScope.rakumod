# A package-less module (no `unit` declarator) whose multi/proto families are
# lexical to this compunit unless imported (#11004).
proto sub bms-proto(|) is export {*}
multi sub bms-proto(Int) { "int" }
multi sub bms-proto(Str) { "str" }

multi sub bms-multi(Int) is export { "m-int" }
multi sub bms-multi(Str) is export { "m-str" }

multi sub bms-private(Int) { "p-int" }
multi sub bms-private(Str) { "p-str" }

sub bms-call-private($x) is export { bms-private($x) }
sub bms-private-block() is export { -> $x { bms-private($x) } }

class BmsBox is export {
    method go($x) { bms-private($x) }
}

# A candidate whose typed named default is computed at BEGIN time; it cannot
# be picked by a plain trial bind from another compunit.
proto sub bms-digest($, :$seed) is export {*}
multi sub bms-digest(Str $s) { samewith $s.encode }
multi sub bms-digest(blob8 $b, blob32 :$seed = BEGIN blob32.new(7)) { "digest:{$b.elems}:{$seed[0]}" }
