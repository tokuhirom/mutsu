use Test;

# A proto's `{*}` that defers with callsame walks the remaining
# candidates of that name in dispatch order. The candidate gather runs on the
# base-name index, not a scan of every registered routine (#17553, found in
# Red's operator-heavy migration verifier); this pins that the walk still sees
# typed, untyped and many-arity candidates, and nothing of other names.

plan 6;

proto sub f(|) {*}
multi sub f(Int $x)  { "Int(" ~ callsame() ~ ")" }
multi sub f(Cool $x) { "Cool(" ~ callsame() ~ ")" }
multi sub f($x)      { "Any" }
multi sub f($x, $y)  { "two" }
sub f-unrelated($x)  { "unrelated" }

is f(42), 'Int(Cool(Any))', 'callsame walks typed then untyped candidates';
is f("s"), 'Cool(Any)', 'a narrower start skips the Int candidate';
is f(1, 2), 'two', 'arity two picks its own candidate';

proto sub infix:<±>(|) {*}
multi sub infix:<±>(Int $a, Int $b) { "ii," ~ callsame() }
multi sub infix:<±>(Numeric $a, Numeric $b) { "nn" }
is 1 ± 2, 'ii,nn', 'callsame from an operator candidate reaches the next one';

module M {
    proto sub g(|) is export {*}
    multi sub g(Str $s) is export { "Str," ~ callsame() }
    multi sub g($s) is export { "any" }
}
import M;
is g("x"), 'Str,any', 'an imported proto walks its own candidates';

my @seen;
for ^200 { @seen.push: f($_) }
is @seen.unique.elems, 1, 'repeated dispatch keeps one answer';
