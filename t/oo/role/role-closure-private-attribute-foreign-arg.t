use Test;

# A closure written in a role method carries the role as its package, and it
# may read the role's `$!attr`s (they belong to the composing class) whatever
# it is called with. The "backdoor private attribute" guard, which rejects a
# free-standing method touching `$!attr` on a foreign invocant, treated the
# role as a non-class package and refused a call whose first argument was an
# instance of some other class — Cro::HTTP's session and auth middleware died
# with "Cannot access private attribute from outside class Supply".

plan 3;

class Other { }
role R {
    has $.x = 5;
    method direct() { my &c = -> $o { $!x }; c(Other.new) }
    method handed() { -> $o { $!x } }
}
class C does R { }

is C.new.direct, 5, 'a role closure called with a foreign instance reads the attribute';
is C.new.handed()(Other.new), 5, 'the same closure called from outside';
is C.new.handed()(Supply.from-list(1)), 5, 'and with a Supply argument';
