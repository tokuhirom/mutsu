use Test;

# `Metamodel::AttributeContainer`'s `rw` / `set_rw`: the `is rw` class trait
# is recorded on the metaclass and read back by `.^rw` as a native 1/0.

plan 10;

class C is rw { has $.a }
is C.^rw, 1, 'class declared `is rw` reports .^rw == 1';
is C.^rw.WHAT.^name, 'Int', '.^rw is an Int';
is C.new.^rw, 1, '.^rw on an instance reads its class flag';

class D { has $.a }
is D.^rw, 0, 'plain class reports .^rw == 0';
is Int.^rw, 0, 'core class reports .^rw == 0';

my $n := Metamodel::ClassHOW.new_type(:name<N>);
$n.^compose;
is $n.^rw, 0, 'new_type type is not rw';

my $m := Metamodel::ClassHOW.new_type(:name<M>);
is $m.^set_rw, 1, '.^set_rw returns 1';
is $m.^rw, 1, '.^set_rw sets the flag';

class MyHOW is Metamodel::ClassHOW { }
my constant T = MyHOW.new_type(:name<T>);
T.^compose;
is T.^rw, 0, 'a user ClassHOW subclass inherits .^rw';

role R is rw { }
dies-ok { R.^rw }, 'a role metaclass has no .^rw';
