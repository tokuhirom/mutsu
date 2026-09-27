use Test;

# A type object passed to a module's custom EXPORT hook can carry a method
# whose body reads a lexical from the importing compilation unit. The hook
# itself runs in the module's environment, but that caller method must retain
# access to its defining lexical (META::constants 0.0.6 exposed this gap).

plan 1;

my constant %META = value => 'from caller';
class ProvideMeta {
    method meta() { %META }
}

use lib 't/lib';
use ExportHookCallerMethod ProvideMeta;

is EXPORTED-VALUE, 'from caller',
    'a caller method invoked by sub EXPORT retains its lexical environment';
