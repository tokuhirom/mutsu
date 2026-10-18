use Test;

# A `use` inside a routine body runs on every call. After the first run of a
# `use` whose import only binds routines lexically, a later run replays what
# the import wrote instead of importing again (#13906); every run must still
# see the same names, and none of them may outlive the routine. Checked
# against rakudo.
plan 10;

use lib 't/lib';

sub inc-of($x) { use UseInBlockRerun; uibr-inc($x) }
is (^5).map({ inc-of($_) }).join(','), '1,2,3,4,5', 'every call reaches the lexically imported routine';

sub qualified-of($x) { use UseInBlockRerun; UseInBlockRerun::uibr-qualified($x) }
is (1..3).map({ qualified-of($_) }).join(','), '10,20,30', 'every call is granted the module package';

sub closure-of($x) { use UseInBlockRerun; -> { uibr-inc($x) } }
my @closures = (1..3).map({ closure-of($_) });
is @closures.map({ $_() }).join(','), '2,3,4', 'closures made by later calls keep the import';

sub sym-of($x) { use UseInBlockRerun; &::('uibr-inc')($x) }
is (1..3).map({ sym-of($_) }).join(','), '2,3,4', 'a by-name lookup sees the import on every call';

sub my-of() { use UseInBlockRerun; MY::.keys.grep(/uibr/).sort.join(',') }
my @mys = (^3).map({ my-of() });
is @mys.unique.elems, 1, 'MY:: lists the import on every call';

sub bump-twice() { use UseInBlockRerun; uibr-bump(); uibr-bump() }
bump-twice();
is bump-twice(), 4, 'a module that also exports a variable imports on every call';

dies-ok { EVAL 'uibr-bump()' }, 'the routine is not visible after the calls';
dies-ok { EVAL 'UseInBlockRerun::uibr-qualified(1)' }, 'nor is the module package';

sub uibr-inc($x) { "own $x" }
sub own-after { use UseInBlockRerun; uibr-inc(1) }
own-after() for ^3;
is own-after(), 2, 'inside the routine the import wins over the unit\'s routine';
is uibr-inc(1), 'own 1', 'outside it the unit\'s routine answers';
