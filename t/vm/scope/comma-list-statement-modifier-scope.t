use v6;
use Test;

# A trailing statement modifier on a comma-separated list of expressions
# gates the WHOLE list, not just its last element: `$a++, $b++ if COND` runs
# neither or both (verified against `raku`). mutsu used to split such a
# statement into an unconditional prefix (everything but the last
# comma-element) followed by a conditionally-modified last element only —
# so `$index--, last if COND;` decremented `$index` on every pass through
# the enclosing loop regardless of COND, while only `last` itself was
# actually gated.
#
# Found via HTTP::Server::Async, whose header-parsing loop
#     while $index++ < $data.elems - 4 {
#         $index--, last if $data[$index] == $rn[0] && ...;
#     }
# relies on the decrement and the `last` firing together: with only `last`
# gated, the unconditional `$index--` undid the loop's own postfix `++`
# every single iteration, so `$index` never advanced and the loop spun
# forever without ever finding the header terminator.

plan 6;

{
    my $a = 0;
    my $b = 0;
    $a++, $b++ if False;
    is-deeply (0, 0), ($a, $b), 'a false modifier gates the whole comma list, not just the tail';
    $a++, $b++ if True;
    is-deeply (1, 1), ($a, $b), 'a true modifier runs every element of the list';
}

{
    # The exact HTTP::Server::Async shape: decrement-then-`last` gated by a
    # single condition inside a `while` loop driven by a *different*
    # variable's postfix increment.
    my Int $index = 0;
    my $n = 0;
    while $index++ < 5 {
        last if ++$n > 20;
        $index--, last if False;
    }
    is $index, 6, 'an unconditional (never-true) $index--, last does not perturb the loop counter';
}

{
    my $n = 0;
    for 1..5 {
        $n++;
        $n++, last if $n == 2;
    }
    is $n, 3, 'last inside a gated comma list fires together with the sibling side effect';
}

{
    my $i = 0;
    my @log;
    @log.push('a'), $i++ until $i >= 3;
    is-deeply @log, ['a', 'a', 'a'], 'an until modifier re-runs the whole comma list each pass';
    is $i, 3, 'and every pass increments the sibling counter too';
}

# vim: expandtab shiftwidth=4
