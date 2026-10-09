# #11175: rakudo orders multi candidates by the partial order `is_narrower`
# (per positional parameter: a refinement -- `where`, literal, subset -- beats
# the same nominal type without one; a narrower nominal type beats a wider
# one). Candidates that each win a different parameter are INCOMPARABLE and
# the one declared first wins. Sub and method dispatch must agree.
use Test;

plan 15;

# subs
{
    multi h2($x, Str:D $u where "k") { "lit" }
    multi h2(Numeric $x, Str:D $u) { "typed" }
    is h2(0.1, "k"), "lit", "sub: where-refined wider type declared first wins";

    multi f2(Numeric $x, Str:D $u) { "typed" }
    multi f2($x, 'k') { 'lit' }
    is f2(0.1, 'k'), "typed", "sub: typed declared first beats literal";

    multi a3(Numeric $x, Str $y where "k") { "lit" }
    multi a3(Rat $x, Str $y) { "typed" }
    is a3(0.1, "k"), "lit", "sub: incomparable pair, declaration order";
}

# methods
class C {
    multi method h2($x, Str:D $u where "k") { "lit" }
    multi method h2(Numeric $x, Str:D $u) { "typed" }
    multi method f2(Numeric $x, Str:D $u) { "typed" }
    multi method f2($x, 'k') { 'lit' }
    multi method a3(Any $x, Str:D $u where "k") { "lit" }
    multi method a3(Numeric $x, Str:D $u) { "typed" }
    multi method b3(Numeric $x, Str $y where "k") { "lit" }
    multi method b3(Rat $x, Str $y) { "typed" }
    multi method c3(Rat $x, Str $y) { "typed" }
    multi method c3(Numeric $x, Str $y where "k") { "lit" }
}
is C.h2(0.1, "k"), "lit", "method: where-refined wider type declared first wins";
is C.f2(0.1, "k"), "typed", "method: typed declared first beats literal";
is C.a3(0.1, "k"), "lit", "method: explicit Any is related to every type";
is C.b3(0.1, "k"), "lit", "method: incomparable pair, first declared";
is C.c3(0.1, "k"), "typed", "method: incomparable pair, first declared (reversed)";
is C.new.h2(0.1, "k"), "lit", "method on an instance";

# reversing the declaration order flips the answer: they are ties
class D {
    multi method f2($x, 'k') { 'lit' }
    multi method f2(Numeric $x, Str:D $u) { "typed" }
}
is D.f2(0.1, "k"), "lit", "method: literal declared first wins";

# comparable candidates are unaffected
class E {
    multi method d(Int $x where * > 0) { "pos" }
    multi method d(Int $x) { "int" }
    multi method e(Int $x) { "int" }
    multi method e($x where * > 0) { "pos" }
    multi method g(Int $x, Str $y) { "IS" }
    multi method g(Cool $x, Str $y where "k") { "CSk" }
}
is E.d(5), "pos", "refinement beats the same type";
is E.d(-5), "int", "refinement falls through when it does not bind";
is E.e(5), "int", "nominal Int beats a where on an untyped parameter";
is E.g(5, "k"), "IS", "incomparable, declared first";
is E.g("a", "k"), "CSk", "only one candidate binds";
