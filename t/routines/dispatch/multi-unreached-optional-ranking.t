use Test;

# An optional positional the call supplies no argument for still orders multi
# candidates by its DECLARED type, as rakudo's tiers do: a type that conforms
# to the other's is narrower, unrelated types tie, and two tied candidates
# that both bind are ambiguous. Declaring optionals is a yes/no narrowness
# step, not a count (#12852). Expected values from rakudo.

plan 16;

multi sub g(Int $a?) { "I" }
multi sub g(Str $a?) { "S" }
throws-like { g() }, X::Multi::Ambiguous,
    'unrelated types at an unreached optional tie';
is g(1),   'I', 'a reached argument still picks Int';
is g('x'), 'S', 'a reached argument still picks Str';

multi sub h($a?)     { "A" }
multi sub h(Int $a?) { "I" }
is h(), 'I', 'Int is narrower than an untyped optional, whatever the order';

multi sub k(Cool $a?) { "C" }
multi sub k(Int $a?)  { "I" }
is k(), 'I', 'Int is narrower than Cool';

multi sub l(Int $a = 1)  { "I" }
multi sub l(Str $a = '') { "S" }
throws-like { l() }, X::Multi::Ambiguous, 'defaulted parameters tie too';

multi sub m(Int $a?, $b?) { "I" }
multi sub m(Str $a?)      { "S" }
throws-like { m() }, X::Multi::Ambiguous,
    'the optional count does not break the tie';

multi sub n(Int $a, Int $b?) { "I" }
multi sub n(Int $a, Str $b?) { "S" }
throws-like { n(1) }, X::Multi::Ambiguous, 'an unreached second optional ties';

multi sub o(Int $a, $b?) { "I" }
multi sub o(Any $a, Int $b?) { "S" }
throws-like { o(1) }, X::Multi::Ambiguous,
    'narrower on the reached and the unreached parameter each: incomparable';

multi sub p(Int $a, Int $b?) { "I" }
multi sub p(Any $a, Str $b?) { "S" }
is p(1), 'I', 'an unrelated unreached type does not undo a narrower one';

multi sub q(@a?) { "P" }
multi sub q(%a?) { "H" }
throws-like { q() }, X::Multi::Ambiguous, 'Positional and Associative tie';

multi sub r($a?)  { "A" }
multi sub r(@a?)  { "P" }
is r(), 'P', 'an @ parameter is narrower than a $ one';

multi sub s(Int $x, $y?)     { "1" }
multi sub s(Int $x, $y?, $z?) { "2" }
throws-like { s(1) }, X::Multi::Ambiguous, 'one optional more is not wider';

multi sub t(Int $x, Int $y?) { "2" }
multi sub t(Int $x)          { "1" }
is t(1), '1', 'declaring no optional is still narrower';

class C {
    multi method g(Int $a?) { "I" }
    multi method g(Str $a?) { "S" }
    multi method h($a?)     { "A" }
    multi method h(Int $a?) { "I" }
}
throws-like { C.g() }, X::Multi::Ambiguous, 'a multi method ties the same way';
is C.h(), 'I', 'and orders by the declared type the same way';
