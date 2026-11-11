use Test;

# `=:=` / `===` between a role-mixed type object and a plain type object
# answers without rendering the mixin's name unless the other side is a
# composed class. From Monad's `absent(Mu \candidate)`, which tests a cached
# `pun but Some` type against `Nil` and `Any` on every construction.
plan 8;

role M { }
class B { }
my \S = B but M;

nok S =:= Nil, 'a mixin type object is not Nil';
nok S =:= Any, 'nor Any';
nok S =:= B, 'nor the class it was made from';
nok S =:= M, 'nor the role';
ok S =:= S, 'it is itself';
ok S === S.new.WHAT, 'it is the type of its instances';
ok S.new.WHAT =:= S, 'in either order';
nok (B but M) =:= B, 'a fresh composition is not its base';
