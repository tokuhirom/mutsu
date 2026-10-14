use Test;

# `:x($r) ($a, $b)` in RakuAST, measured on rakudo 2026.09: one Parameter with
# `names => ("x",)`, `target => $r` and the trailing signature as its
# `sub-signature`. EVAL of the tree binds the alias and destructures the value.

plan 9;

sub param($src) { $src.AST.statements[0].expression.signature.parameters[0] }

{
    my $p = param(Q[sub f(:x($r) ($a, $b)) { 1 }]);
    is $p.names.join(','), 'x', 'the parameter binds under the name `x`';
    is $p.target.name, '$r', '... into the alias target';
    isa-ok $p.sub-signature, RakuAST::Signature, '... with the trailing signature as its sub-signature';
    is $p.sub-signature.parameters.map(*.target.name).join(','), '$a,$b', '... holding the destructured names';
}
{
    my $p = param(Q[sub f(:x($) (:$k!)) { 1 }]);
    is $p.target.name, '$', 'an anonymous alias target keeps its sigil';
    is $p.sub-signature.parameters[0].names.join(','), 'k', '... and the sub-signature a named leaf';
}

my &f1 = Q[sub (:x($r) ($a, $b)) { "$a-$b" }].AST.EVAL;
is f1(x => (1, 2)), '1-2', 'EVAL: a positional destructure of a named value';
my &f2 = Q[sub (:x(%h) (:$k!, *%rest)) { "$k|{%rest.keys}|{%h.elems}" }].AST.EVAL;
is f2(x => {k => 5, z => 1}), '5|z|2', 'EVAL: a hash alias with a named destructure';
my &f3 = Q[sub (:x($) (:$k!)) { "k=$k" }].AST.EVAL;
is f3(x => {k => 5}), 'k=5', 'EVAL: an anonymous scalar alias';
