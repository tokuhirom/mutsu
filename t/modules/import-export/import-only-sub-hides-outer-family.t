use v6;
use Test;
use lib 't/lib';

# Reduced from Green's t/03-more_concise.t: a block-scoped
# `use Green :harness` imports Green's `only sub ok`, which must hide
# Test's outer `multi sub ok` inside that block and only there.

plan 6;

multi sub probe(Int $x) { "outer multi Int $x" }
multi sub probe(Str $x) { "outer multi Str $x" }

{
    use BlockImportOnlySub :harness;
    is ok(1 == 1), 'imported only-sub True',
        'an imported only-sub hides the outer Test multi inside the block';
    is probe(3), 'imported probe 3',
        'an imported only-sub hides a user-declared outer multi (Int arg)';
    is probe('s'), 'imported probe s',
        'an imported only-sub hides a user-declared outer multi (Str arg)';
}

is probe(3), 'outer multi Int 3', 'the outer multi is back after the block (Int)';
is probe('s'), 'outer multi Str s', 'the outer multi is back after the block (Str)';
ok 1 == 1, 'Test\'s ok is back after the block';
