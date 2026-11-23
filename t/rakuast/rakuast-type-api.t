use Test;
use experimental :rakuast;

plan 18;

# The `RakuAST::Type::*` API raku-doc/doc/Type/RakuAST/Type/*.rakudoc
# documents, as rakudo 2026.11 answers it (#18978).

my $c = RakuAST::Type::Capture.new(RakuAST::Name.from-identifier("T"), :smiley<D>);
is $c.DEPARSE, '::T:D', 'Capture.new with a smiley';
is-deeply $c.definite, True, '... is definite';
is-deeply $c.smiley, 'D', '... and keeps its smiley';
is RakuAST::Type::Capture.from-identifier('T', 'D').DEPARSE, '::T:D', 'from-identifier with a smiley';
is-deeply RakuAST::Type::Capture.from-identifier('T', 'U').definite, False, ':U is not definite';
is-deeply RakuAST::Type::Capture.from-identifier('T').definite, Bool, 'no smiley: the Bool type object';

is-deeply RakuAST::Type::Simple.from-identifier('int').is-native, True, 'int is native';
is-deeply RakuAST::Type::Simple.from-identifier('Int').is-native, False, 'Int is not';
is-deeply RakuAST::Type::Setting.from-identifier('num64').is-native, True, 'a Setting type too';
is-deeply 'my int $x'.AST.statements[0].expression.type.is-native, True, 'a parsed native type';

is-deeply RakuAST::Type::Coercion.new(
    base-type  => RakuAST::Type::Simple.from-identifier("Int"),
    constraint => RakuAST::Type::Simple.from-identifier("Str"),
).is-coercive, True, 'a coercion type is coercive';
is-deeply 'my Int(Str) $x'.AST.statements[0].expression.type.is-coercive, True, '... parsed';
is-deeply 'my Int $x'.AST.statements[0].expression.type.is-coercive, False, 'a plain type is not';
ok RakuAST::Type::Simple.from-identifier('Int').^can('is-coercive'), 'Type::Simple can is-coercive';

is 'my Int:D $x = 1'.AST.statements[0].expression.type.name.canonicalize, 'Int:D',
    'Definedness.name';

my $e = RakuAST::Type::Enum.new(term => RakuAST::QuotedString.new(
    processors => <words val>, segments => (RakuAST::StrLiteral.new("a b c"),)));
is $e.scope, 'our', 'an enum is our-scoped by default';
is $e.DEPARSE, 'enum  <a b c>', 'an anonymous enum';
nok RakuAST::Type::Enum.new(name => RakuAST::Name.from-identifier("E"),
    term => RakuAST::StrLiteral.new("a")).of.defined, 'any term is accepted';
