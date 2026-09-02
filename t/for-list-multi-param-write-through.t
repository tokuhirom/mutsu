use v6;
use Test;

plan 10;

# `for` over a LIST OF SCALAR VARIABLES writes each parameter back to its own
# source variable. A multi-parameter loop chunks the source, so each of the
# chunk's slots has its own source variable — the writeback used to store the
# whole CHUNK over the FIRST source variable, and a list that mixed variables
# with literals got no writeback at all.

{
    my $a;
    my $b;
    for ($a, $b) -> \x, \y { x = 9; y = 8 }
    is-deeply $a, 9, 'the first slot writes back to the first source variable';
    is-deeply $b, 8, 'the second slot writes back to the second source variable';
}

{
    # A mixed list: the literal positions have no source to write back to.
    my $a;
    my $b;
    for $a, 1000, $b, 1000000 -> \x, $value { x = $value }
    is-deeply $a, 1000, 'a mixed list still writes through at slot 0';
    is-deeply $b, 1000000, 'a mixed list still writes through on the second chunk';
}

{
    # A readonly parameter must not overwrite its source.
    my $a;
    my $b;
    for ($a, $b) -> \x, $y { x = 5 }
    is-deeply $a, 5, 'the aliasing parameter writes back';
    nok $b.defined, 'a plain readonly parameter leaves its source alone';
}

{
    # `is rw` parameters behave the same way.
    my $a;
    my $b;
    for ($a, $b) -> $x is rw, $y is rw { $x = 1; $y = 2 }
    is-deeply $a, 1, 'is rw slot 0 writes back';
    is-deeply $b, 2, 'is rw slot 1 writes back';
}

{
    # Regression guard: the single-parameter form is unchanged.
    my $a;
    my $b;
    for $a, $b -> \x { x = 7 }
    is-deeply ($a, $b), (7, 7), 'a single-parameter loop still writes every source';
}

{
    # Three slots, so the chunk index really has to be multiplied by the arity.
    my ($p, $q, $r);
    for ($p, $q, $r) -> \x, \y, \z { x = 1; y = 2; z = 3 }
    is-deeply ($p, $q, $r), (1, 2, 3), 'a three-parameter chunk writes every slot';
}
