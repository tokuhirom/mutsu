use Test;

# A role parameterised at run time (`$obj does R[...]`, `$obj but R[...]`) binds
# the argument VALUES. Composition used to re-read them from the text of the
# pun's name, where a Str such as "A::B" looks like a type name and a Callable
# renders as nothing, so no candidate matched -- or the text did not parse at
# all (#9497).

plan 7;

role R[&f, :$model] { method m { $model }; method g { f() } }

my $model = "Nope::X";
my &cb = { 7 };
my $a = 42;
$a does R[&cb, :$model];
is $a.m, 'Nope::X', 'a named Str argument containing :: binds as the Str';
is $a.g, 7, 'and the positional Callable is the Callable';

role N[:$model] { method m { $model } }
my $b = 1;
$b does N[:model<A::B::C>];
is $b.m, 'A::B::C', 'a literal named argument containing :: also binds';

my $c = 1 but R[{ 8 }, :model("A B")];
is $c.m, 'A B', '`but` with a named Str argument';
is $c.g, 8, '`but` with a block argument';

role P[$x] { method m { $x } }
my $v = [5, 7, 9];
is-deeply (1 but P[$v]).m, [5, 7, 9], 'an itemized Array is one argument, not its element count';
is P[$v].^name, 'P[Array]', 'and parameterises the role type with Array';
