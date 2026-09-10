use v6;
use Test;

# A parameter may carry a destructuring sub-signature AND a `where`
# post-constraint at the same time: `sub f($x ($a, $b) where { ... })`.
# The two are independent — the `where` tests the parameter's own value
# (as `$_`), the sub-signature unpacks it — but the parameter parser used
# to stop as soon as it had consumed the sub-signature, so the trailing
# `where` was left unconsumed and the whole signature failed to parse.
#
# The four spellings that already parsed (sub-signature alone, `where`
# alone, positional and named) are pinned here alongside the four that did
# not, so a future change cannot fix one half by breaking the other.

plan 16;

# --- the four spellings that always worked ---------------------------------

sub sig-only($x ($a, $b)) { "$a/$b" }
is sig-only((1, 2)), '1/2', 'sub-signature alone';

sub where-only($x where { $_ > 10 }) { "$x" }
is where-only(42), '42', 'where alone';

sub named-sig-only(:$x! ($a, $b)) { 'called' }
is named-sig-only(x => (1, 2)), 'called', 'named parameter, sub-signature alone';

sub named-where-only(:$x! where { $_ > 10 }) { "$x" }
is named-where-only(x => 42), '42', 'named parameter, where alone';

# --- the four that used to fail to parse -----------------------------------

sub pos-sig-where($x ($a, $b) where { $_.elems == 2 }) { "$a/$b" }
is pos-sig-where((3, 4)), '3/4', 'positional: sub-signature then where';

sub pos-slurpy-where($x (*@a) where { $_.elems == 2 }) { 'called' }
is pos-slurpy-where((5, 6)), 'called', 'positional: slurpy sub-signature then where';

# The named spellings are pinned at the level this ticket is about -- the
# signature parses and the call binds. What the constraint's topic is for a
# named parameter carrying a sub-signature is a separate, pre-existing gap:
# mutsu still reads `:$x! ($a, $b)` as the RENAME form `:x(:$a)` rather than
# as destructuring, so neither `$x` nor the inner variables bind the way
# rakudo binds them -- tracked as #7865. Do not pin that behavior here.
sub named-sig-where(:$x! ($a, $b) where { True }) { 'called' }
is named-sig-where(x => (7, 8)), 'called', 'named: sub-signature then where';

sub named-slurpy-where(:$x! (*@a) where { True }) { 'called' }
is named-slurpy-where(x => (9, 10)), 'called', 'named: slurpy sub-signature then where';

# --- the other two sub-signature spellings ---------------------------------

sub anon-sig-where(($a, $b) where { $_.elems == 2 }) { "$a/$b" }
is anon-sig-where((11, 12)), '11/12', 'anonymous sub-signature then where';

sub bracket-sig-where(@a [$x, $y] where { $_.elems == 2 }) { "$x/$y" }
is bracket-sig-where([13, 14]), '13/14', 'bracketed sub-signature then where';

# --- the constraint is enforced, not merely parsed --------------------------

sub enforced($x ($a, $b) where { $_.elems == 2 }) { "$a/$b" }
is enforced((15, 16)), '15/16', 'a value meeting the constraint binds';
dies-ok { enforced((1, 2, 3)) }, 'a value failing the constraint does not bind';

multi picky($x ($a, $b) where { $_.elems == 2 }) { 'two' }
multi picky($x) { 'other' }
is picky((1, 2)), 'two', 'multi dispatch picks the constrained candidate';
is picky((1, 2, 3)), 'other', 'multi dispatch falls through when the where fails';

# --- traits and a default value still work alongside the sub-signature ------

sub with-trait($x ($a, $b) is copy where { $_.elems == 2 }) { "$a/$b" }
is with-trait((17, 18)), '17/18', 'is-trait between the sub-signature and the where';

sub with-default($x ($a, $b) = (19, 20)) { "$a/$b" }
is with-default(), '19/20', 'default value after a sub-signature';

# vim: ft=raku
