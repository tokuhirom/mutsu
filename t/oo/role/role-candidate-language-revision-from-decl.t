use Test;

# A role candidate's language revision is the one its *declaration* was compiled
# under, snapshotted at parse time — exactly like a class's. Reading the parser's
# ambient revision when the declaration executes instead reports whichever
# revision happens to be active then, which for a role in a `use`d module is the
# importer's, so every candidate of a multi-module role group collapsed to the
# same revision.

plan 3;

use lib $?FILE.IO.parent.add('lib').Str;
use RoleRev6c;
use RoleRev6e;

is-deeply RevRole.^candidates.map( ~ *.^language-revision ), <c e>,
    'each candidate keeps the revision of the module that declared it';
is RevRole.new.^language-revision, 'c', 'pun of the 6.c candidate stays 6.c';
is RevRole[Str].new.^language-revision, 'e', 'pun of the 6.e candidate stays 6.e';
