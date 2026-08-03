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

plan 6;

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

done-testing;
