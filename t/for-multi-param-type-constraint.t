use v6;
use Test;

# A multi-parameter `for` loop (`-> $k, $v`) binds its parameters with plain
# assignments emitted into the body prefix, and an assignment is type-checked
# against the interpreter's *name-keyed* constraint map. That map is not
# block-scoped, so an unrelated `my Int $v` / `state Int $v` anywhere in the
# program made every `-> $k, $v` loop reject non-Int values with
# "Type check failed in assignment to $v; expected Int".
#
# Found via the vendored Cro::HTTP suite: OpenSSL::Stack's `state Int $v` broke
# `for %headers.kv -> $k, $v { $resp.append-header($k, $v) }` in
# t/http-rawbodyparserselector.rakutest, which could not run a single test.

plan 14;

sub declares-typed-v() { my Int $v = 42; $v }
is declares-typed-v(), 42, "the typed lexical itself still works";

my @seen;
for ("content-length", 10, "transfer-encoding", "identity") -> $k, $v {
    @seen.push("$k=$v");
}
is @seen.join("|"), "content-length=10|transfer-encoding=identity",
    "a multi-param loop binds values of any type after an unrelated typed lexical";

sub state-typed-v() { state Int $v = 7; $v }
is state-typed-v(), 7, "a state-typed lexical still works";

my @seen2;
for ("a", 1, "b", "two") -> $k, $v { @seen2.push("$k=$v") }
is @seen2.join("|"), "a=1|b=two",
    "a `state Int \$v` elsewhere does not constrain a loop parameter either";

# The enclosing lexical keeps its own constraint after the loop has shadowed it.
my Int $v = 1;
for (1, 2) -> $k, $v { }
dies-ok { $v = "boom" }, "the shadowed outer `my Int \$v` is still type-checked after the loop";
lives-ok { $v = 5 }, "...and still accepts an Int";

# The loop parameter is a fresh binding that shadows the enclosing lexical
# only for the loop's duration -- the outer VALUE, not just its type
# constraint, must be back afterward.
# (todo/tickets/for-multi-param-shadow-clobbers-outer-lexical.md)

# Untyped outer scalar.
{
    my $w = "outer";
    for (1, 2) -> $k, $w { }
    is $w, "outer", "untyped outer \$w survives being shadowed by a multi-param loop";
}

# Typed outer scalar.
{
    my Int $t = 99;
    for (1, 3, 2, 4) -> Int $a, Int $t { }
    is $t, 99, "typed outer Int \$t survives being shadowed by a multi-param loop";
}

# Sigilless outer binding.
{
    my \s = "outer-s";
    for (1, "x", 2, "y") -> $a, \s { }
    is s, "outer-s", "sigilless outer \\s survives being shadowed by a multi-param loop";
}

# Nested loop reusing the outer loop's own parameter name must not corrupt it.
{
    my @rounds;
    for 1..2 -> $i {
        for (10, 20, 30, 40) -> $a, $i { }
        @rounds.push($i);
    }
    is-deeply @rounds, [1, 2],
        "an inner multi-param loop reusing the outer loop's \$i does not clobber it";
}

# A global-by-name write (no local slot for the shadowed name in this frame)
# must not leak either -- the same root cause as the @/% case below, just
# reached via a routine with no other reference to the name.
# (todo/tickets/for-multi-param-array-hash-shadow-clobbers-outer-container.md)
{
    sub f() { for <a b c>.kv -> $j, $u { } }
    my $j = 42;
    f();
    is $j, 42, "a multi-param loop in a routine does not leak into an outer \$j via a global write";
}

# `@`/`%`-sigil multi-param loop variables are their own fresh per-iteration
# lexicals, not aliases of a same-named outer `@`/`%` -- an `@`/`%` slot holds
# a mutable container, so a plain per-iteration assignment (as opposed to a
# fresh declaration) mutated the OUTER container in place.
# (todo/tickets/for-multi-param-array-hash-shadow-clobbers-outer-container.md)

# Outer array shadowed by an `@`-sigil multi-param.
{
    my @arr = (100, 200);
    for 1, [10, 20], 2, [30, 40] -> $a, @arr { }
    is-deeply @arr, [100, 200], "outer \@arr survives being shadowed by an \@-sigil multi-param loop";
}

# Outer hash shadowed by a `%`-sigil multi-param.
{
    my %h = (x => 1);
    for 1, {y => 2}, 3, {z => 3} -> $a, %h { }
    is-deeply %h, {x => 1}, "outer %h survives being shadowed by a %-sigil multi-param loop";
}

# The loop parameter itself still binds a fresh value each iteration.
{
    my @arr = (100, 200);
    my @seen;
    for 1, [10, 20], 2, [30, 40] -> $a, @arr {
        @seen.push(@arr.join('-'));
    }
    is @seen.join('|'), "10-20|30-40",
        "the \@-sigil multi-param itself still binds each iteration's own value";
}

done-testing;
