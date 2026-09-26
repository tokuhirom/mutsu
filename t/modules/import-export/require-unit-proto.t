use Test;
use lib 't/lib';

plan 2;

# BDD::Behave::Playwright loads a namespaced unit with this export shape after
# its bare-file wrapper has already populated the caller's GLOBAL namespace.
# A lazily required unit module may export a proto whose name is already
# occupied by a bare-file module imported by the caller.  The package-local
# declaration must not be rejected as a GLOBAL redeclaration.
use RequireProtoWrapper;
lives-ok { require ::('RequireProtoTarget') },
    'dynamic require permits a unit proto after an imported same-named wrapper';
my \M = (require ::('RequireProtoTarget'));
is M::{'&shared-name'}(42), 42,
    'the required unit module registers its own proto/multi family';
