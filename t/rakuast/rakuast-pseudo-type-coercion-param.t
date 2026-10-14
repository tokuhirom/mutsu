use Test;

# `::?CLASS() $x` in RakuAST, measured on rakudo 2026.09: a parameter whose
# `type` is a `Type::Coercion` over the pseudo-type `::?CLASS` (a plain
# `Type::Simple`), not a `::T` type capture.

plan 6;

my $src = Q:to/X/;
class Holder {
    has $.a;
    method coerced(::?CLASS() $x) { $x.a }
}
X

my $ast = $src.AST;
my $param = $ast.statements[0].expression.body.body.statement-list.statements[1].expression.signature.parameters[0];
isa-ok $param.type, RakuAST::Type::Coercion, 'the parameter type is a coercion';
isa-ok $param.type.base-type, RakuAST::Type::Simple, '... over a simple type';
is $param.type.base-type.name.canonicalize, '::?CLASS', '... named after the pseudo-type';
is $param.type-captures.elems, 0, '... with no type capture';
is $param.target.name, '$x', '... and the written target';

my $class = $ast.EVAL;
is $class.new(a => 5).coerced($class.new(a => 7)), 7, 'EVAL of the tree keeps the coercion parameter callable';
