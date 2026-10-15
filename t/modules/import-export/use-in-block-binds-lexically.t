use Test;

# ADR-8751: a `use` binds its routines lexically in the block that contains
# it. Code the block encloses -- a closure, a named sub, one created before the
# `use` -- keeps the import after the block exited, and the import wins over
# the importing unit's own routine of the same name.
plan 8;

use lib 't/lib';

sub uib-g() { 'main g' }
sub uib-named(|) { 'main named' }

sub mk-closure { use UseInBlockM; -> { uib-g() } }
is mk-closure()(), 'M g', 'a closure keeps its block\'s import after the block exited';

sub mk-before { my &c = -> { uib-g() }; use UseInBlockM; &c }
is mk-before()(), 'M g', 'a closure created before the use in its block sees the import';

sub mk-named { use UseInBlockM; sub inner { uib-g() }; &inner }
is mk-named()(), 'M g', 'a named sub declared in the block sees the import';

sub mk-nested { use UseInBlockM; -> { -> { uib-g() } } }
is mk-nested()()(), 'M g', 'nested closures inherit the import';

sub mk-args { use UseInBlockM; -> |c { uib-named(|c, :tag<wrapped>) } }
is mk-args()(), 'M named wrapped', 'slipped and named arguments reach the import';

{
    use UseInBlockM;
    is uib-g(), 'M g', 'inside the block the import wins over the unit\'s routine';
}
is uib-g(), 'main g', 'after the block the unit\'s own routine answers again';

sub uib-other { uib-g() }
{
    use UseInBlockM;
    todo 'a callee written outside the block still sees the registry alias (ADR-8751 stage 1)';
    is uib-other(), 'main g', 'a callee written outside the block does not see the import';
}
