use lib 't/lib';
use Test;

# `require` of a module that is already loaded does not run its body again,
# even when the module declares only types named apart from its file (Red's
# `model`s in a test library, whose relationship traits ran again on every
# re-run and corrupted the models).

plan 4;

use RequireTypesOnly;
is $*REQUIRE-TYPES-ONLY-RUNS, 1, 'the use loaded it once';

sub look-up() {
    my $t = ::('RequireTypesOnlyThing');
    if !$t && $t ~~ Failure {
        require ::('RequireTypesOnly');
    }
    ::('RequireTypesOnlyThing')
}
is look-up().hi, 'hi', 'the type resolves after the require';
is $*REQUIRE-TYPES-ONLY-RUNS, 1, 'require of the loaded module did not run its body again';
require RequireTypesOnly;
is $*REQUIRE-TYPES-ONLY-RUNS, 1, '... nor did a plain require';
