use Test;

# ADR-8751 stage 2: a block-scoped `use` binds an imported user prefix,
# postfix or circumfix operator in the block's lexical import set, as it does
# an infix one. A closure called after the block exited keeps it, the import
# wins over the unit's own operator of that name inside the block, and a
# callee written outside the block does not see it.
plan 12;

use lib 't/lib';

sub prefix:<¢>($x) { "main-p($x)" }
sub postfix:<¥>($x) { "main-q($x)" }
sub circumfix:<⌊ ⌋>($x) { "main-c($x)" }
sub uibao-prefix-callee { ¢1 }
sub uibao-postfix-callee { 1¥ }
sub uibao-circumfix-callee { ⌊1⌋ }

sub mk-prefix { use UseInBlockAffixOps; -> { ¢2 } }
is mk-prefix()(), 'P(2)', 'a closure keeps the prefix operator';

sub mk-postfix { use UseInBlockAffixOps; -> { 2¥ } }
is mk-postfix()(), 'Q(2)', 'a closure keeps the postfix operator';

sub mk-circumfix { use UseInBlockAffixOps; -> { ⌊2⌋ } }
is mk-circumfix()(), 'C(2)', 'a closure keeps the circumfix operator';

sub mk-amp { use UseInBlockAffixOps; -> { &prefix:<¢>(3) ~ &postfix:<¥>(4) } }
is mk-amp()(), 'P(3)Q(4)', '&prefix:<op> and &postfix:<op> read the import';

{
    use UseInBlockAffixOps;
    is ¢1, 'P(1)', 'inside the block the imported prefix wins';
    is 1¥, 'Q(1)', 'so does the imported postfix';
    is ⌊1⌋, 'C(1)', 'and the imported circumfix';
    is uibao-prefix-callee(), 'main-p(1)', 'a callee outside the block does not see the prefix';
    is uibao-postfix-callee(), 'main-q(1)', 'nor the postfix';
    is uibao-circumfix-callee(), 'main-c(1)', 'nor the circumfix';
}
is ¢1, 'main-p(1)', 'after the block the unit\'s own prefix answers again';
is 1¥, 'main-q(1)', 'and its own postfix';
