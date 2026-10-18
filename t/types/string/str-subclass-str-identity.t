use Test;

# `Str.Str` and `Str.Stringy` are `self`, so a `Str` subclass answers them with
# the instance (ADR-11276 §9.183), as `Int.Int` answers an `Int` subclass.

plan 8;

class S is Str { }
my $s = S.new(value => "x");
isa-ok $s.Str, S, '.Str is the instance';
isa-ok $s.Stringy, S, '.Stringy too';
is ~$s, 'x', 'prefix ~ reads the payload';
is "$s!", 'x!', 'interpolation too';
is $s.uc, 'X', 'the other methods run on the payload';

class Own is Str { method Str { 'own' } }
is Own.new(value => 'y').Str, 'own', "a class's own Str wins";
class Child is Own { }
is Child.new(value => 'z').Str, 'own', '... and is inherited';
ok S.new(value => 'q') eq 'q', 'eq compares the payload';
