use Test;

plan 11;

# A sized buffer (`buf8`, `blob8`, …) is an instance whose `.^name` is the
# PARAMETERIZED spelling `Buf[uint8]`, while Raku source spells it `buf8`.
# The multi-dispatch type-distance table matched only the short spelling and
# only through `value_type_name` (which answers the generic "Any" for every
# instance), so every sized buffer scored the "unrelated type" distance: a
# `Buf`/`Blob`/`buf8`/`blob8` candidate lost to a bare `@`-parameter one.
# Digest::SHA3's `multi KeccakF1600(blob8 $state)` was shadowed by its
# `multi KeccakF1600(@lanes)` sibling, so the permutation never ran.

my buf8  $b8   .= new: 1, 2, 3, 4;
my blob8 $bl8  .= new: 1, 2, 3, 4;
my       $buf   = Buf.new(1, 2);

{
    multi A(@l)       { "array" }
    multi A(blob8 $x) { "blob8" }
    is A($b8), "blob8", 'blob8 beats a bare @-parameter for a buf8';

    multi B(@l)      { "array" }
    multi B(Blob $x) { "Blob" }
    is B($b8), "Blob", 'Blob beats a bare @-parameter for a buf8';

    multi C(@l)     { "array" }
    multi C(Buf $x) { "Buf" }
    is C($b8), "Buf", 'Buf beats a bare @-parameter for a buf8';

    multi D(@l)      { "array" }
    multi D(buf8 $x) { "buf8" }
    is D($b8), "buf8", 'buf8 beats a bare @-parameter for a buf8';

    multi E(@l)     { "array" }
    multi E(Buf $x) { "Buf" }
    is E($buf), "Buf", 'the unsized Buf case still works';

    multi F(@l)      { "array" }
    multi F(Blob $x) { "Blob" }
    is F($bl8), "Blob", '...and a blob8 argument';
}

# Declaration order must not decide any of these — the typed candidate wins
# whichever way round it is declared.
{
    multi G(blob8 $x) { "blob8" }
    multi G(@l)       { "array" }
    is G($b8), "blob8", 'reversing the declarations does not change the winner';
}

# Narrower still wins over wider inside the family.
{
    multi H(Blob $x) { "Blob" }
    multi H(buf8 $x) { "buf8" }
    is H($b8), "buf8", 'buf8 is narrower than Blob for a buf8';

    multi I(Blob $x) { "Blob" }
    multi I(Buf $x)  { "Buf" }
    is I($b8), "Buf", 'Buf is narrower than Blob for a buf8';
}

# An unrelated type still loses to the @-parameter.
{
    multi J(@l)     { "array" }
    multi J(Int $x) { "Int" }
    is J($b8), "array", 'an unrelated constraint does not match at all';
}

# The typecheck these distances have to agree with.
ok $b8 ~~ blob8, 'a buf8 does the blob8 role';
