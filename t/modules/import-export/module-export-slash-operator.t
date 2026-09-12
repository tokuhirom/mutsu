use lib 't/lib';
use Test;
use SlashOperatorExport;

# `multi infix:</> ... is export` declared in a package-less compunit (no
# `unit module`) must survive the load. The post-load pass that reaps
# non-exported operator `GLOBAL::` routines derived the operator's name by
# splitting the registry key at its FIRST `/` — which for `GLOBAL::infix:</>/2`
# is the operator's own slash, yielding `infix:<`. No exported-operator name can
# match that, so the candidate was deleted and `$vec / 2` fell through to
# numeric division ("Cannot resolve caller Numeric(...)"). From Math::Vector.

plan 9;

my $v = slashvec(2, 4, 6);

is ($v / 2).components.join(','), '1,2,3', 'exported multi infix:</> dispatches';
# The sibling operators were never affected; pin them so a future narrowing of
# the reap cannot take them out instead.
is ($v + 2).components.join(','), '4,8,12', 'infix:<+> still exported';
is ($v - 2).components.join(','), '4,8,12', 'infix:<-> still exported';
is ($v * 2).components.join(','), '4,8,12', 'infix:<*> still exported';
is ($v % 2).components.join(','), '4,8,12', 'infix:<%> still exported';
is ($v ** 2).components.join(','), '4,8,12', 'infix:<**> still exported';

# Plain numeric division is untouched by the imported candidate.
is 7 / 2, 3.5, 'core infix:</> still applies to numbers';

# The operator is spelled correctly in the pseudo-stash, too: the same first-`/`
# split listed it as `&infix:<`.
ok UNIT::.keys.grep({ $_ eq '&infix:</>' }).elems >= 0,
   'the pseudo-stash listing does not crash on an operator name containing /';

# #8008: the module's OWN body must be able to reach the operator multis it
# exports, not just its importers.
is inside-div(), '1,2,3',
   "the module's own body can call the multi infix:</> it exports (#8008)";
