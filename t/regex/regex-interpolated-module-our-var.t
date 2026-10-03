use lib 't/lib';
use Test;

# `<$var>` inside a module's regex reads the module's own file-scope `our`
# variable even when the module's routine is called from a scope that never
# imported the module directly. Found via the CSS::Minifier distribution.

use RegexModuleOurVarUser;

plan 3;

is user-recolor('x red y BLUE'), 'x C y C', 'a substitution in a closure sees the module variable';
ok user-has-named('a blue sky'), 'a match sees the module variable';
nok user-has-named('green'), 'and still fails to match what it should not';
