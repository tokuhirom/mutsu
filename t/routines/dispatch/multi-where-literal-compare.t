use Test;

# A `where` clause or `subset` predicate that is one comparison of the value
# against an integer literal (`* < 100`) is answered without running its
# body (#10111). The answer must be the comparison's own, at every boundary,
# and a user operator, or a value that is not a plain Int, must still reach
# the predicate's ordinary evaluation.

plan 12;

subset Lt100 of Int where * < 100;
subset Le100 of Int where * <= 100;
subset Gt100 of Int where * > 100;
subset Ge100 of Int where * >= 100;
subset Eq100 of Int where * == 100;

is-deeply (99, 100, 101).map({ $_ ~~ Lt100 }).List, (True, False, False), 'subset where * < N';
is-deeply (99, 100, 101).map({ $_ ~~ Le100 }).List, (True, True, False), 'subset where * <= N';
is-deeply (99, 100, 101).map({ $_ ~~ Gt100 }).List, (False, False, True), 'subset where * > N';
is-deeply (99, 100, 101).map({ $_ ~~ Ge100 }).List, (False, True, True), 'subset where * >= N';
is-deeply (99, 100, 101, -100).map({ $_ ~~ Eq100 }).List, (False, True, False, False),
    'subset where * == N';

multi size(Lt100 $x)                  { 'small' }
multi size(Int $x where * < 10_000)   { 'medium' }
multi size(Int $x where * >= -5)      { 'non-negative-ish' }
multi size($x)                        { 'other' }
is-deeply (0, 99, 100, 9_999, 10_000, -3, -6, 2**70, 1/2, '5').map({ size($_) }).List,
    <small small medium medium non-negative-ish small small non-negative-ish other other>.List,
    'multi candidates by subset and where';

subset Positive of Int where * > 0;
is-deeply (-1, 0, 1, 2**70).map({ $_ ~~ Positive }).List, (False, False, True, True),
    'a BigInt value takes the ordinary comparison';

sub bounded(Int $n where * <= 3) { $n }
is bounded(3), 3, 'a plain sub binds a value meeting its where';
throws-like { bounded(4) }, X::TypeCheck::Binding::Parameter,
    'a plain sub rejects a value failing its where';

{
    # A user `infix:<<>` in scope decides the comparison, not the core one.
    sub infix:«<»(Int $a, Int $b) { $a > $b }
    subset Reversed of Int where * < 10;
    sub reversed-where(Int $x where * < 10) { 'bound' }
    ok 20 ~~ Reversed, 'a user infix:<<> decides a subset predicate';
    nok 5 ~~ Reversed, 'a user infix:<<> rejects through a subset predicate';
    is reversed-where(20), 'bound', 'a user infix:<<> decides a where clause';
}
