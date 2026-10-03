use v6;
use Test;

# From SION (zef distribution): its encoder's `multi method value` had
# `Date:D`, `Associative:D` and `Mu:D` candidates and recursed with
# `self.value(.value, $depth)` over a hash's pairs. A hash element is a
# container, and the candidate ranking looked at the container instead of its
# Date, so `Date:D` lost to the catch-all (or tied with it: ambiguous).
plan 7;

class Enc {
    multi method value(Date:D $v, Int $) { 'date' }
    multi method value(Associative:D $v, Int $d) {
        $v.pairs.map({ self.value(.value, $d) }).join(',')
    }
    multi method value(Any:D $v, Int $) { 'any' }
}

my %h = a => Date.new(2020, 1, 1);
is Enc.new.value(%h, 0), 'date', 'recursing over a hash picks Date:D';
is Enc.new.value(%h<a>, 0), 'date', 'hash element';
is Enc.new.value(%h.values[0], 0), 'date', '.values element';
is Enc.new.value(%h.pairs[0].value, 0), 'date', 'Pair value';
is Enc.new.value(Date.new(2020, 1, 1), 0), 'date', 'plain value';
is Enc.new.value({a => Date.new(2020, 1, 1)}, 0), 'date', 'hash literal';

class Enc3 {
    multi method value(Mu:U, Int $) { 'nil' }
    multi method value(Date:D $v, Int $) { 'date' }
    multi method value(Associative:D $v, Int $d) {
        $v.pairs.map({ self.value(.value, $d) }).join(',')
    }
    multi method value(Mu:D $v, Int $) { 'mu' }
}
is Enc3.new.value({a => Date.new(2020, 1, 1)}, 0), 'date', 'with a Mu:D fallback (was ambiguous)';
