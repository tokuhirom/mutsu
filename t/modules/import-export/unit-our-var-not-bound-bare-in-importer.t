use lib 't/lib';
use Test;

plan 13;

# #11009: a `unit module`'s `our` variables are package variables. The module
# body runs in the loading scope's env, but its own bare bindings must not
# survive there: rakudo reaches them as `$Unit::x`, or bare only through a
# real import of an `is export` one.

# A block-scoped `use` is preloaded at the head of the unit; the block that
# says it never runs, yet nothing may become visible file-wide.
ok ::('$uobn-exp') ~~ Failure, 'a never-run block-scoped use exposes no exported our';
if False { use UnitOurBareName; }

{
    use UnitOurBareName;
    is $uobn-exp, 'exp', 'the block-scoped use imports the exported our into the block';
    is uobn-read(), 'our/exp/1 2/1', "the module's routines read their own our variables";
}
ok ::('$uobn-exp') ~~ Failure, 'the import stays in the block';

{
    use UnitOurBareName;
    ok ::('$uobn-our') ~~ Failure, 'an unexported our $x is not bound bare in the importer';
    ok ::('@uobn-arr') ~~ Failure, 'an unexported our @a is not bound bare in the importer';
    ok ::('%uobn-hash') ~~ Failure, 'an unexported our %h is not bound bare in the importer';
    is $uobn-exp, 'exp', 'an exported our $x is imported';
    is $UnitOurBareName::uobn-our, 'our', 'the package-qualified name still reaches it';
    is-deeply @UnitOurBareName::uobn-arr, [1, 2], 'the package-qualified array still reaches it';
    is uobn-bump(), 'our+', "the module's own write lands on its our \$x";
    is $UnitOurBareName::uobn-our, 'our+', 'the package-qualified name sees that write';
    $UnitOurBareName::uobn-our = 'set';
    is uobn-read(), 'set/exp/1 2/1', "a write through the qualified name reaches the module's routines";
}
