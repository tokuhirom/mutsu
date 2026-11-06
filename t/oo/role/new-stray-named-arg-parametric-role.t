use Test;

plan 5;

# Mu.new ignores a named argument that names no attribute, also when the
# class composes a parametric role.
role Q[Int $t] { }
class B does Q[2] { has $.x = 1 }
is-deeply B.new(:0reserved), B.new, 'stray named arg ignored (parametric role)';
ok B.new(:0reserved) eqv B.new, 'eqv holds';

role R[::T] { has $.y = 2 }
class C does R[Int] { has $.x = 1 }
ok C.new(:0reserved) eqv C.new, 'type-parameter role with an attribute';
is C.new(:y(5), :0reserved).y, 5, 'a role attribute is still set from its named arg';

class D does Q[3] { has $.x = 1; submethod TWEAK { } }
ok D.new(:0reserved) eqv D.new, 'with a TWEAK submethod';
