use Test;

# `::(q<Foo>) $x` in RakuAST, measured on rakudo 2026.09: the parameter's
# `type-captures` hold a `Type::Capture` whose Name is a single
# `Name::Part::Expression` over the expression, and EVAL of the tree keeps the
# indirect type lookup working.

plan 7;

class Foo { }

my $src = Q:to/X/;
class Bar { method bar(::(q<Foo>) $x) { 'bar called' } }
X

my $ast = $src.AST;
my $param = $ast.statements[0].expression.body.body.statement-list.statements[0].expression.signature.parameters[0];
is $param.type-captures.elems, 1, 'one type capture';
my $capture = $param.type-captures[0];
isa-ok $capture, RakuAST::Type::Capture, '... a Type::Capture';
my $parts = $capture.name.parts;
is $parts.elems, 1, '... whose name has one part';
isa-ok $parts[0], RakuAST::Name::Part::Expression, '... an expression part';
isa-ok $parts[0].expr, RakuAST::QuotedString, '... over the written string';
is $param.target.name, '$x', '... and the target is kept';

my $bar = $ast.EVAL;
is $bar.new.bar(Foo.new), 'bar called', 'EVAL: the indirect capture accepts an instance of the named type';
