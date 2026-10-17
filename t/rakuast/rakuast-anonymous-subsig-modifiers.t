use Test;
use experimental :rakuast;

# Anonymous destructuring parameters keep their outer default, constraint and
# traits beside the nested signature (measured on rakudo 2026.09).
plan 9;

sub param(Str $src) {
    $src.AST.statements.head.expression.signature.parameters.head
}

my $where = param(Q[sub f(($a, $b) where { $_.elems == 2 }) { }]);
ok $where.where.defined, 'the anonymous destructuring parameter retains where';
like $where.gist, /'optional      => False,' .* 'where'/,
    'where follows the optional field';
is EVAL(Q[sub f(($a, $b) where { $_.elems == 2 }) { "$a/$b" }; f((1, 2))].AST),
    '1/2', 'the sub-signature and where constraint lower together';
throws-like {
    EVAL(Q[sub f(($a, $b) where { $_.elems == 2 }) { "$a/$b" }; f((1, 2, 3))].AST)
}, Exception, 'the outer where constraint is enforced';

my $default = param(Q[sub f(($a, $b) = (3, 4)) { }]);
ok $default.default.defined, 'the anonymous destructuring parameter retains default';
nok $default.optional.defined, 'a default does not add an optional field';
my $trait = param(Q[sub f(($a, $b) is copy) { }]);
isa-ok $trait.traits[0], RakuAST::Trait::Is, 'the anonymous parameter retains traits';
like $trait.traits[0].gist, /'from-identifier("copy")'/, 'the trait name is preserved';
is EVAL(Q[sub f(($a, $b) is copy) { "$a/$b" }; f((5, 6))].AST),
    '5/6', 'a trait-bearing sub-signature round-trips';
