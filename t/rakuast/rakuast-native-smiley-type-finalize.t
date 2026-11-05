use v6;
use Test;

plan 3;

# A native type word with a definiteness smiley (`int:D`, `num:_`) is resolved
# where it stands by finalization (ADR-15760 slice 2).

sub f(int:D $x, num:_ $y) { $x + $y.Int }
is f(2, 1e0), 3, 'definite and any-definedness native parameters';
is int:D.^name, 'int:D', 'the smiley type object names itself';
dies-ok { EVAL 'f(Int, 1e0)' }, 'a type object does not bind an int:D parameter';
