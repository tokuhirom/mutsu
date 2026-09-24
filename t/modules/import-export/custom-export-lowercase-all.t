use Test;

# Text::MathematicalCase uses the conventional lowercase EXPORT::all stash in
# its custom EXPORT hook to implement selective imports. The lowercase alias
# is the same export stash as EXPORT::ALL. Code-valued exported constants
# must be visible there too; List::MoreUtils uses this for aliases.
plan 8;

use lib 't/lib';
use CustomExportLowercaseAllFixture <exported>;

ok &exported.defined, 'the lowercase ALL stash installs the named code object';
is exported(), 'from lowercase all', 'EXPORT::all resolves the ALL export stash';

use CustomExportLowercaseAllFixture <exported-alias>;

ok &exported-alias.defined, 'the lowercase ALL stash installs an exported alias';
is exported-alias(), 'from lowercase all', 'EXPORT::all resolves exported code-valued constants';

use CustomExportLowercaseAllFixture <exported-multi>;
is exported-multi(42), 'int:42', 'the lowercase ALL stash preserves the integer multi candidate';
is exported-multi('answer'), 'str:answer', 'the lowercase ALL stash preserves the string multi candidate';

use CustomExportLowercaseAllFixture :all;
ok &exported.defined, 'the lowercase ALL stash imports ordinary exports for :all';
is exported(), 'from lowercase all', ':all keeps custom and ordinary exports together';
