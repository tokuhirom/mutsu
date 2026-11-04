use Test;
use lib 't/lib-enum-term-over-sub';
use EnumTermOverSub;

# From the TimeUnit distribution: `minutes` is both an enum member of the
# module and an exported sub. A bare `minutes` is the enum term (rakudo looks
# the term up before falling back to a routine call); `minutes(30)` is the sub.
plan 4;

is minutes(30), 'sub 30', 'a call with an argument list runs the sub';
is unit-key(minutes), 'minutes', 'the bare word is the enum member';
sub capture(|c) { c.raku }
is capture(3, minutes), '\(3, UnitName::minutes)', 'bare word as a call argument';
is (minutes).^name, 'UnitName', 'the term is an enum value';
