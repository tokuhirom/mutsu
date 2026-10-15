use Test;

# ADR-8751 stage 2: a block-scoped `use` binds an imported multi family (as
# its dispatcher value) and an imported binary operator in the block's
# lexical import set. Code the block encloses keeps them after the block
# exited, a callee written outside the block does not see the multi family,
# and a multi the block declares itself merges with the imported family.
plan 10;

use lib 't/lib';

multi uibm-f(Int $x) { "main int $x" }
sub infix:<+%+>($a, $b) { "main $a+%+$b" }
sub uibm-callee { uibm-f(1) }

sub mk-multi { use UseInBlockMulti; -> { uibm-f('k') } }
is mk-multi()(), 'M str k',
    'a closure keeps the imported multi family after the block exited';

sub mk-amp { use UseInBlockMulti; &uibm-f }
is mk-amp()('q'), 'M str q', 'so does the family\'s &name value';
is mk-amp()(5), 'M int 5', 'which dispatches over every imported candidate';

sub mk-bare { use UseInBlockMulti; -> { uibm-bare(3) } }
is mk-bare()(), 'M bare 3', 'a family exported without a proto too';

sub mk-op { use UseInBlockMulti; -> { 3 +%+ 4 } }
is mk-op()(), 'M 3+%+4', 'a closure keeps an imported binary operator';

{
    use UseInBlockMulti;
    is uibm-f(1), 'M int 1', 'inside the block the import hides the unit\'s family';
    is uibm-callee(), 'main int 1',
        'a callee written outside the block does not see the imported family';
}
is uibm-f(1), 'main int 1', 'after the block the unit\'s own family answers again';

{
    use UseInBlockMulti;
    multi uibm-f(Num $x) { "local num $x" }
    is uibm-f(2), 'M int 2', 'a multi the block declares merges with the import...';
    is uibm-f(1e0), 'local num 1', '...and answers for its own signature';
}
