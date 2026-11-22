use Test;

# A multi winner a keyed stage program picked binds through the light bind
# without its parameter constraints being checked again (ADR-18580 D2): a
# subset, a user class with a `where`, `UInt`. Deferral out of such a winner, a
# call with no matching candidate, and the parameter's own value must stay as
# rakudo has them. Each call shape runs twice, so the second pass takes the
# cached light path.
plan 14;

subset Small of Int where * < 100;
multi s(Small $x) { "small " ~ callsame() }
multi s(Int $x)   { "int $x" }
for ^2 -> $pass {
    is (s(5), s(500), s(5), s(7)).join('|'), 'small int 5|int 500|small int 5|small int 7',
        "callsame out of a subset-typed winner (pass $pass)";
}

multi only(Small $x) { "only $x" }
for ^2 -> $pass {
    is only(3), 'only 3', "a lone subset candidate (pass $pass)";
}
throws-like { only(300) }, X::Multi::NoMatch, 'no candidate still dies';

class Foo { has $.v }
multi g(Foo $f where *.v > 1) { "big {$f.v}" }
multi g(Foo $f) { "foo {$f.v}" }
for ^2 -> $pass {
    is (g(Foo.new(:v(5))), g(Foo.new(:v(0))), g(Foo.new(:v(9)))).join('|'),
        'big 5|foo 0|big 9', "a user class with a where (pass $pass)";
}

multi n(int $x where * > 0) { "pos" }
multi n(Int $x) { "int" }
for ^2 -> $pass {
    is (n(5), n(-5), n(True)).join('|'), 'int|int|int',
        "a native int candidate (pass $pass)";
}

multi u(UInt $x) { $x.WHAT.^name }
multi u(Int $x)  { "Int!" }
for ^2 -> $pass {
    is (u(True), u(3), u(-3)).join('|'), 'Bool|Int|Int!',
        "the bound value keeps its type (pass $pass)";
}

multi w(Small $x, Str $s) { "$x $s" }
multi w(Int $x, Str $s)   { "big $x $s" }
for ^2 -> $pass {
    is (w(1, "a"), w(1000, "b")).join('|'), '1 a|big 1000 b',
        "two parameters (pass $pass)";
}
is s(1_000_000_000_000_000_000_000), 'int 1000000000000000000000',
    'a big integer falls back to the full check';
