use Test;

# A smiley on a type capture constrains the parameter's definedness: rakudo
# reads `::T:D $x` as `::T Any:D $x` (#19020).

plan 7;

sub d(::T:D $x) { T.^name }
is d(3), 'Int', '::T:D binds an instance and captures its type';
throws-like { d(Int) }, X::Parameter::InvalidConcreteness, '::T:D refuses a type object';

sub u(::T:U $x) { T.^name }
is u(Str), 'Str', '::T:U binds a type object and captures it';
throws-like { u('s') }, X::Parameter::InvalidConcreteness, '::T:U refuses an instance';

sub a(::T:_ $x) { T.^name }
is a(Int), 'Int', '::T:_ takes a type object';
is a(5), 'Int', '... and an instance';

is &d.signature.raku, ':(::T Any:D $x)', 'the signature shows Any:D';
