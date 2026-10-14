# A module's `sub EXPORT` reads `UNIT::`, the compunit's own scope, from
# inside its own frame and from a closure: both must see the unit's
# `my constant &name`, its `my $x` and its `my sub`s (#13272).
use Test;
use lib 't/lib';

plan 4;

use UnitStashSelective <us-alpha us-beta>;

is us-alpha(1), 'alpha:1', 'a selected `my constant &name` is imported';
is us-beta(2), 'beta:2', 'and a second one, selected from a closure';
is unit-stash-report(), 'True|True True|$us-plain &us-alpha &us-beta &us-helper',
    'UNIT:: from the EXPORT frame and from a closure in it';
nok MY::<&us-helper>:exists, 'an unselected name is not imported';
