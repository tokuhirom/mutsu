use Test;
use lib 't/lib';

plan 3;

# A file module's `our &greet is export` is bound in the package, not in the
# scope that loaded the module: only an import brings it into scope.
need ImportEnvScope;

nok (try { EVAL '&greet.defined' }), 'need alone does not bind the exported code variable';
ok  (try { EVAL '&ImportEnvScope::greet.defined' }), 'the package still holds it';
{
    import ImportEnvScope;
    is greet(), 'hello', 'an import binds it';
}
