use v6;
use Test;
use nqp;

# From MUGS::Games (MUGS::Util::StructureValidator): a schema holding
# `(A | B) but Optional` is read through nqp::getattr on Junction's
# `$!type` / `$!eigenstates`, and `Optional.ACCEPTS` uses `.^mixin_base`.

plan 9;

my $j = (Int | Str);
is nqp::getattr(nqp::decont($j), Junction, '$!type'), 'any', 'junction $!type';
is nqp::getattr(nqp::decont($j), Junction, '$!eigenstates').elems, 2, 'junction $!eigenstates';
is nqp::getattr(nqp::decont(all(1, 2, 3)), Junction, '$!type'), 'all', 'all type';
is nqp::getattr(nqp::decont(one(1, 2)), Junction, '$!type'), 'one', 'one type';
is nqp::getattr(nqp::decont(none(1, 2)), Junction, '$!type'), 'none', 'none type';

role Optional { method ACCEPTS(Mu $o) { self.^mixin_base.ACCEPTS($o) } }

my $v = (Callable | [ Pair:D ]) but Optional;
is nqp::getattr(nqp::decont($v), Junction, '$!type'), 'any', 'type through a mixin';
is $v.^mixin_base.^name, 'Junction', '.^mixin_base of a junction mixin';
is (Bool but Optional).^mixin_base.^name, 'Bool', '.^mixin_base of a type mixin';
is-deeply (5 ~~ ((Int | Str) but Optional)), False, 'ACCEPTS delegates to the base type';

done-testing;
