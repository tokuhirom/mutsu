use v6;
use Test;

# A role declared inside a class, with a body-level `my` of the same name as one
# of the class's: a method mixed in with `but` / `.^mixin` reads the ROLE's
# variable. The enclosing class's statics were injected over the role's own
# composition bindings. Found with Interval (`Interval[60].succ`). Identical
# under raku.

plan 7;

class C1 {
    my \iv = 1;
    method v { iv }
    my role R[\n] { my \iv = n * 2; method v { iv } }
    method mk($n)  { self.^mixin(R[$n]) }
    method mk2($n) { self but R[$n] }
    method inst($n) { R[$n].new.v }
}
is C1.new.v, 1, 'the class method still reads the class variable';
is C1.inst(5), 10, 'a pun of the role';
is C1.new.mk2(5).v, 10, '`but` with a parametric role';
is C1.mk(5).new.v, 10, '.^mixin with a parametric role';

class C2 {
    my $iv = 1;
    method v { $iv }
    my role R[\n] { my $iv = n * 2; method v { $iv } }
    method mk2($n) { self but R[$n] }
}
is C2.new.mk2(5).v, 10, 'a sigiled variable';

class C3 {
    my \iv = 1;
    my role R { my \iv = 7; method v { iv } }
    method mk2 { self but R }
}
is C3.new.mk2.v, 7, 'a non-parametric role';

class C4 {
    method v { 1 }
    my role R[\n] { my \iv = n * 2; method v { iv } }
    method mk2($n) { self but R[$n] }
}
is C4.new.mk2(5).v, 10, 'no clash at all';
