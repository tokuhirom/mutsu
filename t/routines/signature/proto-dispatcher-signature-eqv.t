use Test;

# A multi dispatcher's own introspection is its proto's, not its candidates':
# `.signature`/`.arity`/`.count` read the declared proto (or the generated
# `(;; Mu |)` one), a package-qualified `&Pkg::proto` handle belongs to `Pkg`,
# and two dispatchers are `eqv` when their `.raku` agree (Rakudo's
# `Any:D eqv Any:D`). Expectations measured against rakudo 2026.07 (#10707);
# rakudo 2026.09 spells the generated proto `(|)`, in `.raku` of the dispatcher
# (pinned in proto-dispatcher-is-one-routine.t) and `.signature` alike.

plan 17;

proto sub f(|) {*}
multi sub f($x) { 1 }
multi sub f($x, $y) { 2 }
is &f.signature.raku, ':(|)', 'a declared proto answers its own signature';
is &f.arity, 0, 'and its arity';
is &f.count, Inf, 'and its count';

proto sub g($a) {*}
multi sub g(Int $a) { 1 }
is &g.signature.raku, ':($a)', 'not the single candidate\'s signature';
is &g.arity, 1, 'arity of a one-parameter proto';

multi sub h(Int $a) { 1 }
multi sub h(Str $a) { 2 }
is &h.signature.raku, ':(;; Mu |)', 'a protoless multi answers the generated proto';
is &h.arity, 0, 'whose arity is 0';
is &h.count, Inf, 'and count Inf';

sub plain($x, $y?) { $x }
is &plain.signature.raku, ':($x, $y?)', 'a plain sub keeps its own signature';

package P {
    our proto sub q(|) {*}
    multi sub q(Int $a) { 1 }
}
is &P::q.cando(\(5)).elems, 1, '&Pkg::proto.cando finds the matching candidate';
is &P::q.cando(\("x")).elems, 0, 'and none for a non-matching capture';
is &P::q.package.^name, 'P', '&Pkg::proto belongs to Pkg';
is &P::q.signature.raku, ':(|)', 'and answers its proto\'s signature';

package A { our proto sub p(|) {*}; multi sub p(Int) { 1 } }
package B { our proto sub p(|) {*}; multi sub p(Str) { 2 } }
ok &A::p eqv &B::p, 'dispatchers with the same .raku are eqv';

sub ff { 1 }
sub gg { 1 }
nok &ff eqv &gg, 'two distinct plain subs are not';
ok &ff eqv &ff, 'a sub is eqv to itself';

is sub (Mu |c) { }.signature.raku, ':(Mu |c)', 'a typed capture parameter shows its type';
