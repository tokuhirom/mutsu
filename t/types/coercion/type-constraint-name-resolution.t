use Test;

# Pins the arms of `Interpreter::resolved_type_capture_name_cow` and of
# `try_coerce_value_for_constraint`, both of which a typed assignment runs on
# every single store.
#
# Those two are shaped around the case where the declared constraint resolves
# to ITSELF (`my int $i`, `my Str $s`, ...), because that is what a hot loop
# does thousands of times per second: the fast answer borrows the name it was
# given instead of re-searching and re-allocating it. Everything below is a
# constraint that does NOT resolve to itself, i.e. one of the arms the fast
# answer has to keep reachable. Each case was checked against rakudo.
plan 9;

# --- a user `subset` still runs its predicate on a lexical store -------------
subset Even of Int where * %% 2;
my Even $even = 4;
is $even, 4, 'a subset-typed lexical accepts a conforming value';
ok (try { my Even $bad = 3; True }).not,
    'a subset-typed lexical still rejects a non-conforming value';

# --- a coercion type still coerces at the store ------------------------------
# `Int()` goes through `parse_coercion_type`, the one arm of
# `try_coerce_value_for_constraint` that sits ABOVE its no-subsets early
# return and must therefore keep firing in a program that declares no subset
# at all.
my Int() $coerced = "42";
is $coerced, 42, 'a coercion-typed lexical coerces its assigned value';
is $coerced.WHAT.^name, 'Int', 'and stores the coerced type, not the source';

# --- a `::T` type capture still resolves -------------------------------------
sub captured(::T $, T $v) { my T $inner = $v; $inner }
is captured(Int, 7), 7, 'a lexical typed by a `::T` capture accepts a match';
ok (try { captured(Int, "s"); True }).not,
    'and still rejects a value the captured type does not admit';

# --- a capture/alias inside a parameterization still resolves ----------------
# The `Foo[Bar]` arm resolves each type argument in turn, and does so even
# when no `::T` capture has ever been bound in the process -- a `constant`
# type alias is resolved there by `type_alias_target`, with no capture in
# sight.
constant AliasedInt = Int;
my Array[AliasedInt] $parameterized = Array[Int].new(1, 2);
is $parameterized.elems, 2,
    'a parameterization whose argument is a `constant` type alias still binds';

# --- a package-scoped nested type is still reached by its leaf name ----------
class Outer {
    class Inner { has $.n = 5 }
    method m() { my Inner $i = Inner.new; $i.n }
}
is Outer.m, 5, 'a nested type is still resolved by its leaf name inside its package';

# --- the identity path itself ------------------------------------------------
# The case the whole fast answer exists for: a native-typed lexical assigned in
# a loop. Nothing exotic, but if the borrowed-identity answer were wrong this
# is what would break first, and everywhere at once.
sub native-loop() {
    my int $i = 0;
    my int $n = 100;
    while $i < $n { $i = $i + 1 }
    $i
}
is native-loop(), 100, 'a native-typed lexical still type-checks and wraps on every store';
