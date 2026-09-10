use Test;

plan 12;

# ^nominalize and the archetypes flags for subsets, coercion types, and
# definite types (JSON::Unmarshal 040/070).

subset Even of Int where * %% 2;
is Even.^nominalize.^name, "Int", 'subset nominalizes to its base';
is Int:D.^nominalize.^name, "Int", 'definite type nominalizes to base';
is Int(Rat).^nominalize.^name, "Int", 'coercion type nominalizes to target';

ok Int:D.HOW.archetypes.nominalizable, 'definite type is nominalizable';
nok Int:D.HOW.archetypes.nominal, 'definite type is not nominal';
ok Int:D.HOW.archetypes.definite, 'definite type reports definite';
ok Int(Rat).HOW.archetypes.coercive, 'coercion type reports coercive';
nok Int.HOW.archetypes.nominalizable, 'plain class is not nominalizable';

# The coercive check must not fire on parens embedded in a where clause of a
# key-typed hash attribute (`Associative[Str{subset ... any("a", "b")}]`).
class KH {
    has Str %.bla{subset :: of Str where any("ble", "blob")}
}
my $t = KH.^attributes[0].type;
nok $t.HOW.archetypes.nominalizable, 'key-typed hash attr type is not nominalizable';
is $t.of.^name, "Str", '.of on key-typed Associative gives the value type';

# `.of` through a %-sigil parameter bound to the parametric type object.
sub probe-of(%json, %x) { %x.of.^name }
is probe-of({}, KH.^attributes[0].type), "Str", '.of via %-param (V{K} form)';
is probe-of({}, Hash[Int]), "Int", '.of via %-param (plain parametric)';
