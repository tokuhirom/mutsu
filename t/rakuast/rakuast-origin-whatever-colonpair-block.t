use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11:
# - a curried term (`*.a`) keeps its own range when a term of the same shape
#   is written later in the statement (`*.a > 1`), and the `ArgList` around
#   them is placed;
# - `:k{...}` holds the block or hash composer itself, `:k({...})` its
#   parentheses, and both are placed.

plan 7;

sub o($n) { $n.origin ?? $n.origin.from ~ '-' ~ $n.origin.to !! '-' }

{
    my $args = Q[my @a; @a.map(*.a, *.a > 1);].AST.statements[1].expression.postfix.args;
    is o($args), '14-26', 'the ArgList';
    is o($args.args[0]), '14-17', 'the curried term written first';
    is o($args.args[1].left), '19-22', 'the same shape inside a later infix';
}

{
    my @pairs = Q[sub f(|) {}; f(:s{ 1 }, :t({ 2 }), :u{ a => 1 })].AST.statements[1].expression.args.args;
    is @pairs.map({ .value.^name.subst('RakuAST::', '') }).join(' '),
        'Block Circumfix::Parentheses Circumfix::HashComposer', ':k{...} and :k({...}) values';
    is @pairs.map({ o(.value) }).join(' '), '17-22 26-33 37-47', 'their ranges';
    is o(@pairs[0].value.body), '17-22', 'the block body';
    is o(@pairs[0].value.body.statement-list), '19-20', 'its statement list';
}
