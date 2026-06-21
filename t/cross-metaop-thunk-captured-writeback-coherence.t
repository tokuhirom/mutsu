use Test;

# An `X` cross meta-operator over a short-circuit op (`Xorelse` / `Xandthen` /
# `Xand` / `Xor`) compiles its right operand to an immediately-invoked thunk
# (`__mutsu_cross_shortcircuit`). Escape analysis never boxes the outer lexicals
# that thunk captures-and-writes, so without recording the thunk's captured-outer
# writes at the call site they are lost once the blanket reconcile is removed
# (`MUTSU_NO_BLANKET_RECONCILE=1`). Verify the captured write is coherent under
# BOTH builds.

plan 6;

{
    my Mu $topic is default(Nil) = 0;
    Nil Xorelse ($topic = $_,);
    ok $topic === Nil, 'Xorelse thunk writes captured outer Mu scalar (Nil topic)';
}

{
    my $seen = 0;
    Nil Xorelse ($seen = 42,);
    is $seen, 42, 'Xorelse thunk write reaches caller slot';
}

{
    my $seen = 0;
    1 Xandthen ($seen = 7,);
    is $seen, 7, 'Xandthen thunk write reaches caller slot';
}

{
    # Captured-outer write through the thunk then re-read after another call
    # (`ok`'s env save/restore would clobber a stale slot without the writeback).
    my $stored = 0;
    Nil Xorelse ($stored = 11,);
    my $probe = "force a call";   # intervening statement
    is $stored, 11, 'captured-outer thunk write survives a later read';
}

{
    my $hit = 0;
    0 Xor ($hit = 99,);
    is $hit, 99, 'Xor thunk write reaches caller slot when left is falsy';
}

{
    my $untouched = 5;
    1 Xor ($untouched = 99,);
    is $untouched, 5, 'Xor thunk does not run when left is truthy (no spurious write)';
}
