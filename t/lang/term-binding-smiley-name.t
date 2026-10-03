use Test;

# A definiteness-smiley type (`Int:D`) is never a constant's name; resolving
# it as a constraint skips the term namespace (#9494). Constants and types
# with those base names still resolve as before.

plan 6;

module M {
    constant Width = 4;
    our sub f(Int:D $n --> Int:D) { $n * Width }
    our sub g(Str:U $s) { $s.^name }
}
is M::f(3), 12, 'Int:D parameter and return inside a package';
is M::g(Str), 'Str', 'Str:U parameter inside a package';
dies-ok { M::f(Int) }, 'Int:D still rejects a type object';

subset Small of Int where * < 10;
sub h(Small:D $x) { $x }
is h(3), 3, 'a smiley on a subset';

constant T = Int;
sub k(T:D $x) { $x + 1 }
is k(1), 2, 'a smiley on a constant type alias';
dies-ok { k(Int) }, 'which still checks definiteness';
