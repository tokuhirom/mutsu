use Test;

# ADR-8751 stage 2: a block-scoped `use` of a module that adds `multi`
# candidates to a built-in operator (`multi prefix:<->(Pt)`) keeps them on the
# registry path, where the operator finds them alongside the built-in. Bound
# in the block's lexical import set, the family's dispatcher held the imported
# candidates alone, so a candidate's own `-$i` on an Int found no match.
plan 6;

use lib 't/lib';

{
    use UseInBlockBuiltinOpCandidates;
    my $p = Pt.new(v => 1);
    is (-$p).v, -1, 'an imported prefix candidate runs, and its body negates an Int';
    is ($p + $p).v, 2, 'an imported infix candidate runs';
    is (-($p + $p)).v, -2, 'the two compose';
    is -5, -5, 'the built-in prefix still answers an Int in the block';
    is 2 + 3, 5, 'and the built-in infix';
}
is -7, -7, 'after the block the built-in is untouched';
