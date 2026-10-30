use Test;

# At CHECK time static::Analyzer reads a unit's declarations (#15610):
# `VarDeclaration::Constant.compile-time-value` is the constant's value,
# `Type::Capture.compile-time-value` an unbound `Any`, and a bare block or a
# pointy block that is a statement answers `is-block-statement`. Expected values
# are rakudo 2026.09's.

plan 10;

constant C = 5;
constant D = Int;
constant L = <a b>;
sub f(::T $v) { }
{ 1 }
-> $p { 1 };
my $w = { 2 };

my (@constants, @captures, %blocks, @closures);
sub walk($node) {
    return unless $node ~~ RakuAST::Node;
    given $node {
        when RakuAST::Type::Capture { @captures.push: .compile-time-value }
        when RakuAST::VarDeclaration::Constant { @constants.push: .compile-time-value }
        when RakuAST::Block | RakuAST::PointyBlock { %blocks{.^name}.push: .is-block-statement; @closures.push: .compile-time-value }
    }
    $node.visit-children({ walk($_) });
}
CHECK { walk($*CU) }

is-deeply @constants, [5, Int, <a b>], 'a constant answers its evaluated initializer';
ok RakuAST::Type::Capture.^can('compile-time-value'), 'Type::Capture can compile-time-value';
ok @captures.all === Any, 'an unbound type capture, when reached, is Any';
ok %blocks<RakuAST::PointyBlock>.all, 'a pointy block statement is a block statement';
ok %blocks<RakuAST::Block>.any, 'a bare block statement is a block statement';
ok %blocks<RakuAST::Block>.grep(!*).elems >= 1, 'the body of a sub or the value of an assignment is not';
ok RakuAST::Block.^can('is-block-statement'), 'Block can is-block-statement';
ok RakuAST::VarDeclaration::Constant.^can('compile-time-value'), 'Constant can compile-time-value';
ok @closures.elems >= 2, 'blocks were reached';
ok @closures.all ~~ Block, 'a block, pointy or not, answers its closure as compile-time-value';
