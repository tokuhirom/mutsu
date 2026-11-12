use Test;
use lib 't/fixtures/require-unit-loader-imports/lib';
use ReqScope::Loader;

# A unit loaded by `require` from inside a routine of another module is
# compiled on its own: the names that module imported (here a `role
# Grammar[...]` and a `class Base`) are not visible to the required unit's
# declarations. EEL's CLI requires a transform module whose `grammar` must
# inherit the core Grammar, not EEL::Slang's `role Grammar[...]`. Identical
# under rakudo.
plan 3;

my $m = load-and-parse('ReqScopeGrammar', 'aaa');
ok $m, 'a grammar in the required unit inherits the core Grammar';
is ~$m, 'aaa', 'and parses';
is load-and-origin('ReqScopeBase'), 'own class', 'a parent name resolves in the required unit, not the loader';
