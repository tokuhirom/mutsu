use v6;
use Test;

plan 8;

# A lowercase type word the bareword classifier resolves to `Type::Simple`
# (the native types) stays the type word the compiler resolves
# (ADR-15760 slice 2).

my uint8 $a = 200;
my int32 $b = -5;
is $a + $b, 195, 'native-typed variables';
is uint16.^name, 'uint16', 'a native type as a term';
is int64.^name, 'int64', 'another native type term';
ok Int ~~ Any, 'a setting type word';
sub f(int $x --> int) { $x + 1 }
is f(4), 5, 'native types in a signature';
my class Local { }
is Local.^name, 'Local', 'a unit class word';
# A user declaration of the name wins.
{
    my \uint8 = 'mine';
    is uint8, 'mine', 'a sigilless term named like a native type';
}
is Nil.gist, 'Nil', 'Nil stays its own literal';
