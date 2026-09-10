use Test;

# Element and slice assignment on a wide buffer used to run the value through
# `to_int` before storing it, and `to_int` saturates a BigInt at `i64::MAX`. A
# `buf64` element at or above 2**63 therefore came back as
# 0x7FFF_FFFF_FFFF_FFFF instead of the value written. `Buf.new` and `.splice`
# were unaffected — they encode straight from the Value — so a buffer could
# disagree with itself depending on how it was filled.
#
# Found via grondilu's Digest dist: `Digest::SHA2`'s sha512 keeps its message
# schedule in a `state buf64 $w` and assigns `$w[$t] = ...`, so every schedule
# word >= 2**63 was clamped and the digest was wrong from round 25 on.

plan 8;

{
    my buf64 $b .= new(1, 2, 3);
    $b[1] = 0xFFFFFFFFFFFFFFFF;
    is $b[1], 0xFFFFFFFFFFFFFFFF, 'buf64 element assignment keeps a full-width uint64';
    is-deeply $b.list, (1, 0xFFFFFFFFFFFFFFFF, 3), 'the other elements are untouched';
}

{
    my buf64 $b .= new(0);
    $b[0] = 0x8000000000000000;
    is $b[0], 0x8000000000000000, 'buf64 element assignment keeps a value at exactly 2**63';
}

{
    my buf64 $b .= new(1, 2, 3);
    $b[0, 2] = 0x8000000000000000, 0xFFFFFFFFFFFFFFFF;
    is-deeply $b.list, (0x8000000000000000, 2, 0xFFFFFFFFFFFFFFFF),
        'buf64 slice assignment keeps full-width uint64 elements';
}

{
    # The element still wraps at its own width, as every other write path does
    # — the fix removes a premature saturation, not the masking.
    my buf32 $b .= new(0, 0);
    $b[0] = 0xFFFFFFFF;
    $b[1] = 0x1_FFFF_FFFF;
    is-deeply $b.list, (0xFFFFFFFF, 0xFFFFFFFF), 'buf32 elements keep their full width and mask';
}

{
    # A `buf64` filled by `.new` and one filled by element assignment must agree.
    my buf64 $a .= new(0xDEADBEEFCAFEBABE, 0xFFFFFFFFFFFFFFFF);
    my buf64 $b .= new(0, 0);
    $b[0] = 0xDEADBEEFCAFEBABE;
    $b[1] = 0xFFFFFFFFFFFFFFFF;
    is-deeply $b, $a, 'element assignment agrees with Buf.new';
    is $b.gist, $a.gist, 'and so does the gist';
}

{
    # Narrow buffers are unchanged.
    my $b = Buf.new(0, 0);
    $b[0] = 300;
    $b[1] = -1;
    is-deeply $b.list, (44, 255), 'byte buffer element assignment still masks to 0..255';
}
