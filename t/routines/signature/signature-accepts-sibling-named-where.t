use Test;

plan 4;

# A destructuring signature's `where` clause may reference a *sibling*
# named parameter by name (`-> (:$x, :$y where $y > $x) {...}`) — the
# mechanism Cro::HTTP::Router's `request-body` uses to pick between
# candidate blocks by signature. `Signature.ACCEPTS`/`~~ Signature` used to
# evaluate each param's `where` clause with only `$_` bound (matching the
# param's own candidate value), so a sibling reference like `$x` inside
# `$y`'s where clause resolved to whatever `$x` happened to be in the
# enclosing scope (usually undefined) instead of the actual candidate.

my &pick-greater = -> (:$x, :$y where $y > $x) { "y=$y x=$x" };
my &pick-lesser  = -> (:$x, :$y where $y <= $x) { "y=$y x=$x" };

my $body = { x => 42, y => 101 };
my $cap = \($body);

ok &pick-greater.signature.ACCEPTS($cap),
    "sibling-referencing where clause sees the matching candidate";
nok &pick-lesser.signature.ACCEPTS($cap),
    "the non-matching sibling candidate is correctly rejected";
is &pick-greater.($body), "y=101 x=42", "the matching block still runs correctly";

# The env must not leak the temporarily-bound sibling values back out.
my $x = "untouched";
&pick-greater.signature.ACCEPTS($cap);
is $x, "untouched", "ACCEPTS does not leak its sibling param bindings into the caller's scope";
