use v6.e.PREVIEW;
use Test;

# 6.e drops the associative MULTISLICE: an all-scalar-keys `%h{1;2}` is a plain
# single-element lvalue/rvalue again, where 6.d and earlier make it a one-element
# `List` (t/multidim-associative-subscript-assign.t pins the 6.d side, and
# roast/S32-hash/multislice-6e.t pins this one across every adverb).

plan 5;

{
    my %h;
    %h{1;2} = 5;
    is %h{1;2}.raku, '5', 'the read hands back the leaf itself';
    is %h{1;2}.VAR.^name, 'Scalar', 'so its .VAR is the leaf container';
    is %h{1;2} + 3, 8, 'and arithmetic sees the leaf, not a 1-element list';
}

# Single-element semantics mean the whole RHS lands at the leaf.
{
    my %h;
    %h{1;2} = [1, 2, 3];
    is %h.raku, '{"1" => ${"2" => $[1, 2, 3]}}', 'the leaf takes the whole array';
}

# The nested-Hash walk itself is version-independent.
{
    my %h;
    %h{1;2;3} = 9;
    is %h.raku, '{"1" => ${"2" => ${"3" => 9}}}', 'integer keys still autovivify hashes';
}
