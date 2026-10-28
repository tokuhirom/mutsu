use Test;

plan 9;

# `callsame` out of a method of a role mixed in with `but` defers to the
# receiver class's multis in RANKED order -- the most specific matching
# candidate -- not in declaration order. Red's `ScopedDriver.map-exception`
# (`callsame` into the SQLite driver's `where`-constrained multis) got the
# role-composed catch-all `multi map-exception($x)` instead, so a unique
# violation inside a transaction was mapped to UnknownError.

class E { has $.code }
role Scoped { method m($x) { callsame } }

class P { multi method m($x) { "any" }; multi method m(E $x) { "E" } }
is (P.new but Scoped).m(E.new(code => 1)), "E", 'the specific multi, declared after the catch-all';
is (P.new but Scoped).m(5), "any", 'the catch-all for an argument the specific one does not take';

class Q { multi method m(E $x) { "E" }; multi method m($x) { "any" } }
is (Q.new but Scoped).m(E.new(code => 1)), "E", 'the specific multi, declared first';

role R1 { multi method m($x) { "role-any" } }
class S does R1 { multi method m(E $x) { "E" } }
is (S.new but Scoped).m(E.new(code => 1)), "E", 'a role-composed catch-all does not outrank the class multi';
is (S.new but Scoped).m(5), "role-any", '... but still answers other arguments';

class W does R1 {
    multi method m(E $x where { .code == 2067 }) { "unique" }
    multi method m(E $x where { .code == 1 }) { "one" }
}
is (W.new but Scoped).m(E.new(code => 2067)), "unique", 'a where-constrained multi wins';
is (W.new but Scoped).m(E.new(code => 1)), "one", '... the other where-constrained multi';
is (W.new but Scoped).m(E.new(code => 3)), "role-any", 'no where matches: the composed catch-all';
is (W.new but Scoped).clone.m(E.new(code => 2067)), "unique", 'also on a clone of the mixin';
