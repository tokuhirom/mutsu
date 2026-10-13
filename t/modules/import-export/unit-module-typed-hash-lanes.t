use Test;
use lib 't/lib';

# Two `unit module`s each declare a file-scope `%seen` and `@log` with their
# own key/element types, and so does the importing scope. Each module's subs
# check their stores against their own module's declaration, and the importer
# keeps its own (#13086). Every expectation was checked against rakudo.

my %seen{Int};
%seen{1} = 1;
my Str @log = <q>;

use UnitTypedLaneA;
use UnitTypedLaneB;

plan 9;

is put-a((1, 2)), 1, "the first module's sub stores a List key into its List-keyed hash";
is put-b({ a => 1 }), 1, "the second module's sub stores a Hash key into its Hash-keyed hash";
is put-a((3, 4)), 2, "the first module's hash keeps its own key type after the second loaded";
is log-a(3), 1, "the first module's Int array accepts an Int";
nok (try log-a('x')).defined, "the first module's Int array still rejects a Str";
is log-b('x'), 1, "the second module's Str array accepts a Str";
is %seen.keys.map(*.^name).join(','), 'Int', "the importer's hash keeps its Int key type";
nok (try { %seen{'s'} = 1; True }), "the importer's hash still rejects a Str key";

# Last: a routine-local typed hash's key type currently leaks into the
# caller's same-named hash on return (#13139).
is local-a('k'), 'Str', "a routine's own same-named hash uses its own key type";
