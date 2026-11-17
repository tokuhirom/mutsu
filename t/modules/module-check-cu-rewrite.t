use lib 't/lib';
use Test;
use CheckRewrittenModule;
use CheckTagGrep;

# A module whose CHECK phase rewrites its `$*CU` (Red's `CHECK red-compile` in
# a module that builds queries) runs from the rewritten tree, as the main
# program does: its mainline, its exported and `our` routines and its
# file-scope lexicals are the tree's. The rewrite used to be ignored for a
# module: every `.grep` below answered 'untagged' (Red t/124).

plan 6;

is CheckRewrittenModule::<@log>.join(','), 'mainline: 42',
    'the mainline runs from the rewritten tree';
is exported-query(), 42, 'an exported routine is the rewritten one';
is CheckRewrittenModule::package-query(), 42, 'an `our` routine is the rewritten one';
is file-scope-lexicals(), '6 8 5',
    'a routine of the rewritten tree reads the file-scope lexicals once the load is over';

# The CHECK-time code object of a routine declared in a `unit module` carries
# that package, as the routine the module installs does: Red compares the two
# by `static_id` before it uses its compiled query.
my $check-time = @CheckTagGrep::seen-above.head;
is $check-time.package.^name, 'CheckRewrittenModule',
    'the CHECK-time routine of a `unit module` sub is declared in that module';
is $check-time.static_id, above-routine().static_id,
    'it has the static_id of the routine the module installs';
