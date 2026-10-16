use Test;
use lib $?FILE.IO.parent(2).add('lib').Str;

# The file-scope `my` variables of a module file with no `unit` declarator are
# the compunit's own: its routines read and write them wherever they are
# called from, and the importer neither sees them nor lends them its own
# same-named lexicals (ADR-12529 §3, slice 24). Checked against rakudo.

plan 12;

my $count = 'importer';
my $label = 'importer label';

use PackageLessFileLexicals;

is pfl-bump(), 1, 'a module routine writes its own file-scope scalar';
is pfl-bump(), 2, 'and the write persists across calls';
is pfl-count(), 2, 'a sibling routine reads the same variable';
is $count, 'importer', 'the importer\'s same-named lexical is untouched';
is pfl-seen(), '1,2', 'a file-scope array is the module\'s own';
is pfl-keys(), 'n1,n2', 'and so is a file-scope hash';
is pfl-label(), 'module', 'a routine called beside a same-named caller lexical reads its own';
is pfl-relabel('rebound'), 'rebound', 'a rebind inside a routine';
is pfl-label(), 'rebound', 'reaches the sibling routines';
is $label, 'importer label', 'and not the importer';
is pfl-in-block(), 2, 'a block inside a module routine reads the file-scope variable';

sub caller-side() { my $count = 'caller'; pfl-count() }
is caller-side(), 2, 'a caller\'s own same-named lexical does not answer for the module';
