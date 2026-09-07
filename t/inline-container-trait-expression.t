use v6;
use Test;

# A container trait (`is SetHash`, `is BagHash`, `is MixHash`, `is Buf`, a
# custom container class) REPLACES the declared container, so a declaration
# used as an expression has to evaluate to the coerced container -- not to the
# plain `Hash`/`Array` that was in the slot before the trait ran.

plan 11;

is (my %q is SetHash).^name, 'SetHash', 'a parenthesised is-SetHash declaration is a SetHash';
ok (my %r is SetHash) ~~ SetHash, '... and smartmatches SetHash';
is (my %s is BagHash).^name, 'BagHash', 'the same for BagHash';
is (my %t is MixHash).^name, 'MixHash', 'the same for MixHash';
is (my @a is Buf).^name, 'Buf', 'the same for an @ declaration with is Buf';

# The statement form was already right; it must stay right.
my %w is SetHash;
is %w.^name, 'SetHash', 'the statement form is unchanged';
nok %w ~~ Set, '... and an immutable Set is still not its type';

# The declaration's own value is what a surrounding expression consumes.
given (my %v is BagHash) { is .^name, 'BagHash', 'a given topic sees the coerced container' }
my $g = (my %h is SetHash);
is $g.^name, 'SetHash', 'an assignment from the declaration sees it too';

# A trait that only annotates the container (rather than replacing it) keeps
# working through the same read-back.
is (my %d is default(42))<nope>, 42, 'is default(...) still travels with the value';
is (my @e is default(7))[9], 7, '... for an array too';
