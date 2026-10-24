use Test;

# Methods rakudo computes from a node's attributes rather than reading one,
# each measured on rakudo 2026.09. Rakudo's own RakuAST::Deparse reads them
# (`$ast.default-scope`, `$name.is-anonymous`, `$call.dispatch`, ...). This
# file passes under both mutsu and raku.

plan 26;

sub expr(Str $src) { $src.AST.statements[0].expression }

# Call::Methodish.dispatch / .dispatcher
my @calls = Q[my $a; sub foo($) {}; $a.?foo; $a.foo; $a."foo"(); $a.*foo].AST.statements[2..*].map(*.expression.postfix);
is @calls.map(*.dispatch).join('|'), '.?|||.*', '.dispatch is the written dispatcher';
is @calls[0].dispatcher, 'dispatch:<.?>', '.dispatcher wraps it';
is @calls[1].dispatcher, '', 'a plain call has no dispatcher';

# VarDeclaration::Simple.original-type
nok expr(Q[my $x]).original-type.defined, 'no type: original-type is Nil';
is expr(Q[my Int $x]).original-type.^name, 'RakuAST::Type::Simple', 'the written type';

# Signature.parameters-initialized
ok expr(Q[sub f($a) {}]).signature.parameters-initialized, 'a written parameter list';
ok expr(Q[sub f {}]).signature.parameters-initialized, 'a parsed sub without parentheses';
ok expr(Q[-> {}]).signature.parameters-initialized, 'a parsed pointy block';
nok RakuAST::Signature.new.parameters-initialized, 'a hand-built Signature without parameters';
ok RakuAST::Signature.new(parameters => ()).parameters-initialized, 'with an empty list';

# default-scope and scope
my %scopes = (
    Q[sub f {}]          => 'my my',
    Q[sub {}]            => 'anon anon',
    Q[class A1 {}]       => 'our our',
    Q[subset S of Int]   => 'our our',
    Q[my $x = 1]         => 'my my',
    Q[enum E <a>]        => 'our our',
    Q[role R {}]         => 'our our',
    Q[my class B {}]     => 'my our',
    Q[constant C = 1]    => 'our our',
    Q[our $y]            => 'our my',
);
for %scopes.sort(*.key) -> (:$key, :$value) {
    my $e = expr($key);
    is "{$e.scope} {$e.default-scope}", $value, "scope / default-scope of '$key'";
}

# Package.augmented
is expr(Q[class A2 {}]).augmented, False, 'a class is not augmented';

# Name.is-anonymous / .is-empty
is RakuAST::Name.from-identifier('a').is-anonymous, 0, 'an identifier is not anonymous';
is RakuAST::Name.new(RakuAST::Name::Part::Empty, RakuAST::Name::Part::Empty).is-anonymous,
    True, '`::` is anonymous';
is RakuAST::Name.from-identifier('a').is-empty, 0, 'an identifier is not empty';
is RakuAST::Name.new.is-empty, 1, 'a name without parts is empty';

ok RakuAST::Call::Method.^can('dispatch'), '.^can finds a computed method';
