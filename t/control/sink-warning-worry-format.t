use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

# #18951: rakudo lists sink-context worries under one "Potential
# difficulties:" header. Each names a sunk operator expression by its source
# text and points at it with a `------>` line, and `[] + 1` is useless too.

plan 6;

is_run '1  +   2; say 1', {
    :0status, :out("1\n"),
    :err(/^ "Potential difficulties:\n    Useless use of 1  +   2 in sink context\n    at " \N+ ":1\n    ------> <BOL><HERE>1  +   2; say 1\n" $/),
}, 'an operator expression is named by its source text and pointed at';

is_run 'my $x; $x; say 1', {
    :0status,
    :err(/'Useless use of $x in sink context' \n '    at ' \N+ \n '    ------> my $x; <HERE>$x; say 1'/),
}, 'a variable, pointed at after what precedes it on the line';

is_run '42; 43', {
    :0status,
    :err({ .comb('Potential difficulties:').elems == 1 && .comb('Useless use').elems == 2 }),
}, 'two worries share one header';

is_run '"a" ~ "b"', { :0status, :err(/'Useless use of "a" ~ "b" in sink context'/) },
    'a string operand keeps its quotes';

is_run 'Int', { :0status, :err(/'Useless use of Int in sink context'/) },
    'a type object is named as written';

is_run '[] + 1', { :0status, :err(/'Useless use of [] + 1 in sink context'/) },
    'an array constructor operand is useless too';
