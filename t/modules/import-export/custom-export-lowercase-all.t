use Test;

# Text::MathematicalCase uses the conventional lowercase EXPORT::all stash in
# its custom EXPORT hook to implement selective imports. The lowercase alias
# is the same export stash as EXPORT::ALL.
plan 2;

use lib 't/lib';
use CustomExportLowercaseAllFixture <exported>;

ok &exported.defined, 'the lowercase ALL stash installs the named code object';
is exported(), 'from lowercase all', 'EXPORT::all resolves the ALL export stash';
