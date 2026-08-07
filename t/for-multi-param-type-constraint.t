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

plan 10;

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

done-testing;
