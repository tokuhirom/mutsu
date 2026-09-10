use v6;
use Test;

# Two parser fixes needed to load dist Algorithm::Diff.

# --- 1. Named parameter alias with an uppercase external name + default -------
# `:NAME($var)` is always a named alias (never a type); the external name may be
# uppercase (`:ASTART($a)`, `:PRUNED($p)`). Previously an uppercase alias with a
# default (`:ASTART($a) = 0`) failed to parse.
sub f(:ASTART($a) = 0, :AFINISH($z) = 10, :MATCHVEC(@m) = []) {
    "$a/$z/{@m.elems}"
}
is f(), '0/10/0', 'uppercase named aliases with defaults use the defaults';
is f(ASTART => 3, AFINISH => 7, MATCHVEC => [1, 2, 3]), '3/7/3',
    'uppercase named aliases bind by external name';

# the alias is a name, not a type: any value binds
sub g(:Int($x) = 'def') { $x }
is g(), 'def', ':Int($x) is an alias, not a type constraint (default)';
is g(Int => 'hi'), 'hi', ':Int($x) binds a Str fine';

# --- 2. `loop {}` followed by a `while`/`until` loop on a new line ------------
# A block-ending `loop {}` is complete; a `while`/`until` on the NEXT line begins
# a separate loop statement (not a `while` modifier).
{
    my $i = 0;
    loop { last if $i > 3; $i++ }

    while $i < 10 { $i++ }
    is $i, 10, 'loop {} then a separate while-loop on a new line';
}

{
    my $j = 0;
    loop { last if $j > 2; $j++ }
    until $j >= 8 { $j++ }
    is $j, 8, 'loop {} then a separate until-loop on a new line';
}

# a same-line `while` modifier on a block is still rejected (Rakudo semantics)
throws-like 'loop { } while $x;', X::Syntax::Confused,
    'loop {} while COND; (same-line modifier) is still an error';

done-testing;
