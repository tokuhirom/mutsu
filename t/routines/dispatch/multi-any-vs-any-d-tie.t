use Test;

# A `:D`/`:U` smiley is no narrowness: `Any` and `Any:D` tie. Without a named
# parameter that is ambiguous; with one, declaration order decides (and a
# required named / more named parameters rank first). Measured against raku.
# Found via Deps t/01-basic.rakutest (`register(::Type Any, :$name, :$priority!)`
# versus `register(Any:D $value, ...)`).

plan 20;

class K {
    multi method a(Any $x, :$p!) { "A1" }
    multi method a(Any:D $x, :$p) { "A2" }
    multi method b(::T Any, :$p!) { "B1" }
    multi method b(Any:D $x, :$p) { "B2" }
    multi method c(Any $x, :$p! where { True }) { "C1" }
    multi method c(Any:D $x, :$p) { "C2" }
    multi method d(Any $x, :$p) { "D1" }
    multi method d(Any:D $x, :$p) { "D2" }
    multi method d2(Any:D $x, :$p) { "D2" }
    multi method d2(Any $x, :$p) { "D1" }
}
my $k = K.new;
is $k.a(1, :p<x>), 'A1', 'required named candidate wins over Any:D';
is $k.b(1, :p<x>), 'B1', 'a ::T Any candidate wins over Any:D';
is $k.c(1, :p<x>), 'C1', 'a required named with where wins over Any:D';
is $k.d(1, :p<x>), 'D1', 'tie with a named parameter: declaration order (Any first)';
is $k.d2(1, :p<x>), 'D2', 'tie with a named parameter: declaration order (Any:D first)';

multi sub s1(Any $x) { 's1a' }
multi sub s1(Any:D $x) { 's1b' }
throws-like { s1(5) }, X::Multi::Ambiguous, 'Any vs Any:D is ambiguous';
multi sub s2($x) { 's2a' }
multi sub s2(Any:D $x) { 's2b' }
throws-like { s2(5) }, X::Multi::Ambiguous, 'an untyped parameter vs Any:D is ambiguous';
multi sub s3(Any $x, Any $y) { 's3a' }
multi sub s3(Any:D $x, Any $y) { 's3b' }
throws-like { s3(5, 6) }, X::Multi::Ambiguous, 'two positionals, Any vs Any:D is ambiguous';
multi sub s4(Any:U $x) { 's4a' }
multi sub s4(Any $x) { 's4b' }
throws-like { s4(Int) }, X::Multi::Ambiguous, 'Any:U vs Any is ambiguous';
is s4(5), 's4b', 'Any:U does not bind a defined value';

multi sub t1(Any $x, :$p) { 't1a' }
multi sub t1(Any:D $x) { 't1b' }
is t1(5), 't1a', 'a candidate with a named parameter ranks first on a tie';
multi sub t2(Any $x, :$p, :$q) { 't2a' }
multi sub t2(Any:D $x, :$p) { 't2b' }
is t2(5), 't2a', 'more named parameters rank first on a tie';

# What still ranks by type.
multi sub n1(Mu $x) { 'n1a' }
multi sub n1(Any:D $x) { 'n1b' }
is n1(5), 'n1b', 'Any:D is narrower than Mu';
multi sub n2(Mu:D $x) { 'n2a' }
multi sub n2(Any $x) { 'n2b' }
is n2(5), 'n2b', 'Any is narrower than Mu:D';
multi sub n3(Int $x) { 'n3a' }
multi sub n3(Any:D $x) { 'n3b' }
is n3(5), 'n3a', 'Int is narrower than Any:D';
multi sub n4(Int:D $x) { 'n4a' }
multi sub n4(Any $x) { 'n4b' }
is n4(5), 'n4a', 'Int:D is narrower than Any';
multi sub n5(Any:D $j, Mu) { 'n5a' }
multi sub n5($j, @x) { 'n5b' }
is n5(1, [1, 2]), 'n5b', 'a Positional parameter outranks (Any:D, Mu)';
multi sub n6(Int:D $x) { 'n6a' }
multi sub n6(Int:U $x) { 'n6b' }
is n6(5), 'n6a', 'Int:D vs Int:U picks by definiteness';
is n6(Int), 'n6b', 'Int:U for a type object';
is-deeply (n3(5), n4(5)), ('n3a', 'n4a'), 'sanity';
