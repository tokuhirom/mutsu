use lib 't/lib';
use Test;

# A slang declarator's `:my $*TALLY` holds what the action of the declared
# method's body set when the declarator's own action runs, also when a
# production is nested inside that body (a parenthesized argument through the
# core `circumfix:sym<( )>`): the slang host's subparse names its start rule with
# the candidate (`routine-declarator:sym<tally>`), whose `:my` declarations are
# recorded under the bare rule name (#16888, EEL 0.3's `pattern`).

plan 3;
use DeclaratorDynvarSlang;
class C {
    tally first { one(1, 2) }
    tally second { two(("x"), 3) }
}
is C.first, 'one', 'the body';
is C.^find_method('first').tally, 'one/2', 'the declarator action sees what the body action set';
is C.^find_method('second').tally, 'two/2', 'also with a parenthesized argument in a later body';
