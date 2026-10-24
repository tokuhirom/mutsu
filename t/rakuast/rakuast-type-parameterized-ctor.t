use Test;
use nqp;
use experimental :rakuast;

plan 14;

# Hand-built Type::Parameterized (measured against rakudo)
my $int = RakuAST::Type::Simple.new(RakuAST::Name.from-identifier("Int"));
my $array = RakuAST::Type::Simple.new(RakuAST::Name.from-identifier("Array"));
my $p = RakuAST::Type::Parameterized.new(
  base-type => $array,
  args      => RakuAST::ArgList.new($int)
);
is $p.^name, 'RakuAST::Type::Parameterized', 'class of the hand-built node';
is-deeply $p.EVAL, Array[Int], 'EVAL of a hand-built Type::Parameterized';
is $p.base-type.^name, 'RakuAST::Type::Simple', 'base-type accessor';
dies-ok { RakuAST::Type::Parameterized.new(base-type => $array, args => 42) },
  'args must be an ArgList';

# A bare Signature node evaluates to the Signature value
sub foo(Int $a, Str $b --> Str:D) { }
my $sig = RakuAST::Signature.new(
  parameters => (
    RakuAST::Parameter.new(
      type   => $int,
      target => RakuAST::ParameterTarget::Var.new(name => '$a')
    ),
    RakuAST::Parameter.new(
      type   => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier("Str")),
      target => RakuAST::ParameterTarget::Var.new(name => '$b')
    ),
  ),
  returns => RakuAST::Type::Definedness.new(
    base-type => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier("Str")),
    definite  => True
  )
);
is-deeply $sig.EVAL, &foo.signature, 'Signature node EVALs to the Signature';

# A constructor argument read from an array element is stored by value
my @parts = "Foo".split('::');
is RakuAST::Name.from-identifier(@parts.head).raku,
  'RakuAST::Name.from-identifier("Foo")', 'array element identifier is a Str leaf';

# Parameterized builtin containers report their composed roles
is Array[Int].^roles.map(*.^name).join(' '),
  'Array::Typed[Int] Positional[Int] Positional Iterable', 'Array[Int].^roles';
is Array[Int].^roles(:!transitive).map(*.^name).join(' '),
  'Array::Typed[Int] Positional Iterable', 'Array[Int].^roles(:!transitive)';
is Hash[Int].^roles.map(*.^name).join(' '),
  'Hash::Typed[Int,Str(Any),Int] Associative[Int] Associative Iterable', 'Hash[Int].^roles';
is Hash[Str,Int].^roles.map(*.^name).join(' '),
  'Hash::Object[Str,Int,Str] Associative[Str] Associative Iterable', 'Hash[Str,Int].^roles';
is Array[Int].^roles.head.^role_arguments.map(*.^name).join(','), 'Int', 'role_arguments of the head role';
is Array[Array[Int]].^roles.head.^role_arguments.map(*.^name).join(','), 'Array[Int]', 'nested argument';

# A meta-object answers nqp::can for its own metamethods
class C { }
ok nqp::can(C.HOW, "roles"), 'nqp::can($type.HOW, "roles")';
nok nqp::can(C.HOW, "no-such-metamethod"), 'nqp::can on an unknown metamethod';
