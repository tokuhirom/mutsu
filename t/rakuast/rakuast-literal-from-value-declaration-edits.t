use experimental :rakuast;
use Test;

# What a slang action builds and edits (#16888, EEL 0.3): `RakuAST::Literal.from-value`
# over any value, a declaration's `lexical-name` and `set-initializer`, and a
# lookup's `set-resolution`. Measured against rakudo 2026.09.

plan 16;

class Foo { has $.x; method hi { "hi " ~ $!x } }

is RakuAST::Literal.from-value(42).^name, 'RakuAST::IntLiteral', 'an Int is an IntLiteral';
is RakuAST::Literal.from-value('s').^name, 'RakuAST::StrLiteral', 'a Str is a StrLiteral';
my $obj = RakuAST::Literal.from-value(Foo.new(:x(3)));
is $obj.^name, 'RakuAST::Literal', 'an object is a plain Literal';
is $obj.value.x, 3, 'which holds the object';
is RakuAST::Literal.from-value(Foo).value.^name, 'Foo', 'a type object';
is RakuAST::Literal.from-value(Nil).^name, 'RakuAST::Literal', 'Nil';
my @a = 1, 2;
is RakuAST::Literal.from-value(@a).value.raku, '[1, 2]', 'the value, not its container';

my $call = RakuAST::ApplyPostfix.new(
    operand => RakuAST::Literal.from-value(Foo.new(:x(7))),
    postfix => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier('hi')));
is EVAL($call), 'hi 7', 'evaluating the literal answers the object';

my $statement = Q|my $x = 1 + 2;|.AST.statements[0];
my $decl = $statement.expression;
is $decl.lexical-name, '$x', 'lexical-name';
ok $decl.initializer ~~ RakuAST::Initializer::Assign, 'the written initializer';
$decl.set-initializer(RakuAST::Initializer);
nok $decl.initializer.defined, 'set-initializer with the type object removes it';
is $statement.DEPARSE, 'my $x', 'the statement no longer initializes';
$decl.set-initializer(RakuAST::Initializer::Assign.new(RakuAST::IntLiteral.new(5)));
is $decl.initializer.expression.value, 5, 'set-initializer with a new one';

my $read = RakuAST::Var::Lexical.new('$x');
nok $read.is-resolved, 'a lookup built by hand is not resolved';
$read.set-resolution($decl);
ok $read.is-resolved, 'set-resolution resolves it';
ok $read.resolution === $decl, 'to the declaration';
