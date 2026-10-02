use Test;

# A routine with `multi` candidates is, as a value, its dispatcher -- the one
# object the short name (an import of it) and the package-qualified `&Pkg::name`
# both denote. It is spelled as its proto, carries the candidates, and answers
# `.is_dispatcher`; each candidate is a `multi sub`. Expectations were measured
# against rakudo 2026.07 (this is the shape BinaryHeap's `is-proto` helper checks
# for `&heapsort` against `&BinaryHeap::Utils::heapsort`).

plan 18;

package BU {
    our proto sub hs(|) is export {*}
    multi sub hs(@a, :$reverse) { 1 }
    multi sub hs(&cmp, @a, :$reverse) { 2 }
}
import BU;

# The qualified reference reaches the candidates, like the imported one.
is &BU::hs.candidates.elems, 2, '&Pkg::proto lists its candidates';
is &hs.candidates.elems, 2, 'and so does the imported &proto';

# Both spell as the proto.
is &hs.raku, 'proto sub hs (|) {*}', 'the imported dispatcher is spelled as its proto';
is &BU::hs.raku, 'proto sub hs (|) {*}', 'and the qualified one';
ok &hs.is_dispatcher, 'the dispatcher answers is_dispatcher';
ok &BU::hs.is_dispatcher, 'the qualified one too';
ok !&hs.candidates[0].is_dispatcher, 'a candidate is not a dispatcher';
is &hs.candidates[0].raku.substr(0, 14), 'multi sub hs (', 'a candidate is a `multi sub`';

# BinaryHeap's `is-proto` helper: the dispatcher and its candidates, compared as
# a list, are the same whichever way the proto is named.
sub is-proto(&got, &expected, $desc = '') {
    is-deeply (&got, |&got.candidates), (&expected, |&expected.candidates), $desc;
}
is-proto &hs, &BU::hs, 'imported &hs is short for &BU::hs';
ok (&hs eqv &BU::hs), 'the two dispatchers are eqv';
ok (&hs.candidates eqv &BU::hs.candidates), 'and so are their candidate lists';

# A proto declares its own signature; a protoless multi gets the generated one.
proto sub g($a) {*}
multi sub g(Int $a) { 1 }
is &g.raku, 'proto sub g ($a) {*}', 'a declared proto shows its own signature';
multi sub h(Int $a) { 1 }
multi sub h(Str $a) { 2 }
is &h.raku, 'proto sub h (;; Mu |) {*}', 'a protoless multi shows the generated proto';
ok &h.is_dispatcher, 'and is a dispatcher';

# A plain routine is not one.
sub plain($x) { $x }
ok !&plain.is_dispatcher, 'a plain sub is not a dispatcher';
is &plain.raku.substr(0, 12), 'sub plain ($', 'and keeps its own spelling';

# A `my`-scoped proto is not in the package stash, so the qualified name finds nothing.
package MyScoped {
    proto sub hidden(|) is export {*}
    multi sub hidden(Int) { 1 }
}
ok !&MyScoped::hidden.defined, 'a my-scoped proto is not reachable as &Pkg::name';
is MyScoped::<&hidden>.defined, False, 'nor through the stash';
