use Test;

# `$x ~~ SomeClass` dispatches `SomeClass.ACCEPTS($x)`: a user `ACCEPTS`
# declared on the class (or inherited) wins over the nominal type test when
# it binds the type-object invocant (#17594).

plan 14;

my class Picky { method ACCEPTS($x) { $x eq 'yes' } }
my class Child is Picky { }

ok  'yes' ~~ Picky, 'a user ACCEPTS answers for the type object';
nok 'no'  ~~ Picky, '... including a False answer';
my $t = Picky;
ok  'yes' ~~ $t, 'through a variable holding the type object';
ok  'yes' ~~ Child, 'an inherited user ACCEPTS';
is ('yes', 'no').grep(Picky).List, ('yes',), 'grep with the type object';
my $hit = '';
given 'yes' { when Picky { $hit = 'when' } }
is $hit, 'when', 'when with the type object';

my class OnlyInt { multi method ACCEPTS(Int $x) { True } }
ok  5 ~~ OnlyInt, 'a multi candidate that binds the topic runs';
nok 'x' ~~ OnlyInt, 'no candidate binds: the nominal test answers';
ok  OnlyInt.new ~~ OnlyInt, '... and accepts an instance';

my class InstOnly { multi method ACCEPTS(InstOnly:D: $x) { $x eq 'inst' } }
nok 'inst' ~~ InstOnly, 'a :D-only candidate leaves the nominal test for the type object';
ok  InstOnly ~~ InstOnly, '... which accepts the type object itself';
ok  'inst' ~~ InstOnly.new, '... while an instance still runs the candidate';

my role R { method ACCEPTS($x) { $x eq 'r' } }
nok 'r' ~~ R, "a role's own ACCEPTS is not consulted for the role type object";
my class DoesR does R { }
ok  'r' ~~ DoesR, '... but a class composing it uses it';
