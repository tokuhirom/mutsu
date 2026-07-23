use v6;
use Test;

# Sigilless-parameter scoping (doc-diff backlog: py-nutshell.rakudoc).
# A sigilless binding must shadow a same-named term constant (e.g. the imaginary
# unit `i`) within its scope, and a single named for-loop param must not leak its
# value to an enclosing binding of the same bare name.

plan 14;

# --- Destructuring sub-signature sigilless params shadow the `i` term ---
# `-> (\i, \j)` binds `i`/`j` as bare-word readonly names; inside the body `i`
# must be the bound value, not the imaginary unit.
is (-> (\i, \j) { i + j })((3, 4)), 7, 'pointy destructure sigilless shadows i';
is (-> (\i, \j) { i + j })((1, 2)), 3, 'pointy destructure sigilless sums bound';

# Nested destructuring.
is (-> (\a, (\b, \c)) { a + b + c })((1, (2, 3))), 6, 'nested destructure sigilless';

# A source-level `my \i` also shadows the term for the destructured params.
{
    my \i = 42;
    is (-> (\i, \j) { i + j })((1, 2)), 3, 'destructure shadows outer sigilless too';
}

# Routine (non-pointy) destructuring sigilless params.
sub add-pair((\i, \j)) { i + j }
is add-pair((3, 4)), 7, 'routine destructure sigilless shadows i';

# Plain (non-destructured) sigilless routine params still work.
sub add2(\i, \j) { i + j }
is add2(3, 4), 7, 'plain sigilless routine params shadow i';

# Sigiled destructuring is unaffected.
is (-> ($a, $b) { $a + $b })((3, 4)), 7, 'sigiled destructure unchanged';

# --- Single named for-loop param must not leak to an outer binding ---
{
    my \x = 10;
    for 1, 2, 3 -> \x { }
    is x, 10, 'sigilless for-param does not leak to outer \x';
}

{
    my $x = 10;
    for 1, 2, 3 -> $x { }
    is $x, 10, 'sigiled for-param does not leak to outer $x';
}

# Int-range for-loop path (a separate VM loop implementation).
{
    my $x = 10;
    for 1 .. 3 -> $x { }
    is $x, 10, 'int-range for-param does not leak to outer $x';
}

# Nested loops reusing the same name restore correctly.
{
    my $i = 99;
    for 1 .. 2 -> $i {
        for 10, 20 -> $i { }
    }
    is $i, 99, 'nested for-loops reusing a name restore the outer';
}

# A LAST phaser must still observe the param at its final iteration value
# (the restore is deferred until after the loop's LAST/post phasers).
{
    my @seen;
    for 1, 2 -> $x { LAST { @seen.push($x) } }
    is-deeply @seen, [2], 'LAST phaser sees the final for-param value';
}

# The loop param genuinely binds each element inside the body.
{
    my @collected;
    my \x = 10;
    for 1, 2, 3 -> \x { @collected.push(x) }
    is-deeply @collected, [1, 2, 3], 'sigilless for-param binds each element';
    is x, 10, 'outer \x still intact after the collecting loop';
}
