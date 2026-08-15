use Test;

# ADR-0019 Phase F box F1 mechanism slice (todo/deep/
# adr0019-f1-f2-introspection-canonical-source.md "Decision (2026-08-14)",
# sequencing point 5): `make_native_method_object` used to hardcode every
# native `Method` Instance's `.signature` as an empty `Signature()` --
# `.^methods`/`.^method_table` on any built-in type answered zero params
# regardless of the method's real arity. It now synthesizes a generic
# `(Owner $:: |)` shape (invocant + raw capture), matching the single most
# common pattern in a raku ground-truth sweep of real native `.signature.gist`
# output across ~280 introspectable (owner, name) pairs (raw-capture was the
# plurality shape; a generic named-catchall and fully-typed explicit params
# were the others, with no single pattern derivable from arity alone -- see
# the linked design doc). This is NOT a claim of exact Rakudo parity: e.g.
# real Rakudo's `Int.^lookup("floor").signature.gist` is
# `(Int:D $:: *%_ --> Int:D)`, a different (also-common) shape. Per-method
# overrides for exact fidelity are a later, separate slice.

for <Int Str Array Hash Range Bool> -> $tyname {
    my $type = ::($tyname);
    my @methods = $type.^methods;
    ok @methods.elems > 0, "$tyname has native methods to check";
    my $still-empty = @methods.first(*.signature.params.elems == 0);
    ok !$still-empty,
        "$tyname has no native method left with an empty Signature()";
}

my $abs = Int.^methods.first(*.name eq 'abs');
is $abs.signature.gist, '(Int $:: |)',
    'native Method Instance signature synthesizes an (Owner $:: |) shape';
is $abs.signature.params.elems, 2,
    'synthesized signature has an invocant param and a capture param';
ok $abs.signature.params[0].invocant,
    'first param is flagged as the invocant';
ok $abs.signature.params[1].capture,
    'second param is flagged as a raw capture';

my $chars = Str.^methods.first(*.name eq 'chars');
is $chars.signature.gist, '(Str $:: |)',
    'a different owner threads its own type into the invocant';

my $push = Array.^methods.first(*.name eq 'push');
is $push.signature.gist, '(Array $:: |)',
    '.^methods native entries get the same synthesized signature regardless of owner';

done-testing;
