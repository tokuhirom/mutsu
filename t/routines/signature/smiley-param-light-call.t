use Test;

# A `:D`/`:U` parameter on a boxed concrete base (`Str:D $s`, `Int:U $t`)
# binds on the light call paths; every verdict and error must stay the
# general binder's. From MCP::Server::Tool::Web's tokenizer, whose small
# `Str:D` helpers ran ~3x slower than their smiley-less twins.
plan 20;

role R { }
class MyInt is Int { }

sub d(Str:D $x) { $x }
sub u(Int:U $x) { $x.^name }
sub two(Int:D $a, Str:D $b --> Str:D) { $b x $a }
sub opt(Int:D $n = 3) { $n * 2 }
sub boo(Int:D $n) { $n.^name }

is d('a'), 'a', ':D admits a value';
is u(Int), 'Int', ':U admits the type object';
is two(2, 'ab'), 'abab', 'two smiley parameters';
is opt(), 6, 'a defaulted :D parameter';
is opt(5), 10, 'a supplied :D parameter';
is boo(True), 'Bool', 'a Bool is a defined Int';
is boo(<42>), 'IntStr', 'an allomorph is a defined Int';
is boo(MyInt.new(7)), 'MyInt', 'a subclass instance is a defined Int';
is u(Int but R), 'Int+{R}', 'a role mixed onto the type object is still a type object';

throws-like { d(Str) }, X::Parameter::InvalidConcreteness,
    message => /'must be an object instance of type \'Str\''/,
    ':D refuses the type object';
throws-like { u(5) }, X::Parameter::InvalidConcreteness,
    message => /'must be a type object of type \'Int\''/,
    ':U refuses a value';
throws-like { d(5) }, X::TypeCheck::Binding::Parameter,
    message => /'expected Str but got Int'/,
    'a value of another type names the base';
throws-like { d(Nil) }, X::TypeCheck::Binding::Parameter,
    message => /'expected Str but got Nil'/,
    'Nil is not a Str';
throws-like { boo(Int but R) }, X::Parameter::InvalidConcreteness,
    ':D refuses a mixed-in type object';

my $e = try { d(Str); 1 } // $!;
like $e.message, /"routine 'd'"/, 'the error names the routine';

for ^3 {
    is d("x$_"), "x$_", "repeated call $_";
}

my @r = (1, 2).map: { two($_, 'z') };
is @r.join(','), 'z,zz', 'called from a block';
is-deeply (^3).map({ opt($_) }).List, (0, 2, 4), 'mapped over a range';
