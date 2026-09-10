use v6;
use Test;

# A coercion-type parameter whose *target* carries a definedness smiley
# (`Identifier:D(Any:D)`, `Int:D(Any)`) must resolve. The role-registration
# type validation stripped the trailing smiley before the coercion parens, so
# for `Identifier:D(Any:D)` nothing was stripped (it ends in `)`), and after
# taking the coercion target `Identifier:D` the smiley was left attached and the
# bare type name never resolved -> "Invalid typename". Surfaced by SQL::Abstract.

plan 4;

# 1. A built-in coercion target with a smiley on both sides.
{
    role R { method f(Int:D(Any) $x) { $x + 1 } }
    class C does R {}
    is C.new.f("41"), 42, 'Int:D(Any) coercion param resolves and coerces';
}

# 2. A user-type coercion target with smileys on both sides.
{
    class Identifier { has $.v; method Str { "id-$!v" } }
    role R2 { method g(Identifier:D(Any:D) $x) { $x.Str } }
    class C2 does R2 {}
    is C2.new.g(Identifier.new(:v(3))), "id-3",
        'Identifier:D(Any:D) coercion param resolves';
}

# 3. The plain (no-smiley) coercion form still works.
{
    role R3 { method h(Int() $x) { $x } }
    class C3 does R3 {}
    is C3.new.h("7"), 7, 'Int() coercion param still resolves';
}

# 4. A genuinely undeclared coercion target is still rejected.
{
    my $ok = False;
    try {
        EVAL 'role RBad { method f(NoSuchXYZ:D(Any) $x) { 1 } }; 1';
        $ok = True;
    }
    nok $ok, 'an undeclared coercion target with a smiley is still rejected';
}
