use v6;
use Test;

# From App::Racoco::Configuration (ecosystem): `multi method of(...)` was read
# as a sub named `method` carrying an `of` trait ("Malformed trait").
plan 4;

class K {
    has $.name;
    multi method of(Str() $n) { self.bless: :name($n) }
    multi method of(Str() $n, Str() $m) { self.bless: :name($n ~ $m) }
    multi method returns(Int $x) { $x + 1 }
}

is K.of("a").name, "a", 'multi method of, one arg';
is K.of("a", "b").name, "ab", 'multi method of, two args';
is K.returns(1), 2, 'multi method returns';

role R[::T] {
    has Str $.name is required;
    multi method of(Str() $name) { self.bless: :$name }
}
class C does R[Int] { }
is C.of("x").name, "x", 'multi method of in a parametric role';
