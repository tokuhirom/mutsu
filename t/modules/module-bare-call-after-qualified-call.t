use Test;

# #13873: a module's bare call of its own `sub bcq-name` resolved to another
# module's same-named `our sub` once the qualified call
# `BareCallQualifiedOther::bcq-name()` had run: that call was cached as what
# the bare name means in the calling package. Checked against rakudo.
plan 3;

use lib 't/lib';
need BareCallQualifiedOwn;

is BareCallQualifiedOwn::sequence(), 'own,other,own',
    'a bare call after a qualified same-named call is still the own sub';
is BareCallQualifiedOwn::later(), 'other,own',
    'so is one in another routine, after a qualified call in a callee';
is BareCallQualifiedOwn::sequence(), 'own,other,own', 'and on a second call';
