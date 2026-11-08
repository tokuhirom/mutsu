use Test;
use experimental :rakuast;

# Model accessors the vendored deparser reads and rakudo answers (each
# measured against rakudo 2026.09): they used to be missing, so .DEPARSE died
# on `$<a>`, `my $ = ...`, `$x."m"()`, `Int()` and `enum`.

plan 12;

my $capture = Q[$<a>].AST.statements[0].expression;
is $capture.^name, 'RakuAST::Var::NamedCapture', 'a named capture';
is $capture.colonpairs.elems, 0, 'Var::NamedCapture.colonpairs is empty without adverbs';

my $anon = Q[my $ = 1].AST.statements[0].expression;
is $anon.^name, 'RakuAST::VarDeclaration::Anonymous', 'an anonymous declaration';
is $anon.traits.elems, 0, 'traits: empty';
is $anon.twigil, '', 'twigil: empty';
nok $anon.original-type.defined, 'original-type: a type object when untyped';
nok $anon.shape.defined, 'shape: undefined';
nok $anon.where.defined, 'where: undefined';
is Q[my Int $ = 1].AST.statements[0].expression.original-type.^name, 'RakuAST::Type::Simple',
    'original-type: the written type';

my $quoted = Q[my $x; $x."foo"()].AST.statements[1].expression.postfix;
is $quoted.args.^name, 'RakuAST::ArgList', 'Call::QuotedMethod.args';

my $coercion = Q[sub f(Int() $x) {}].AST.statements[0].expression.signature.parameters[0].type;
is $coercion.constraint.^name, 'RakuAST::Type::Setting', 'Type::Coercion.constraint defaults to Any';

my $enum = Q[enum Foo <a b>;].AST.statements[0].expression;
is $enum.clean-clone.traits.elems, 0, 'Type::Enum.clean-clone.traits';
