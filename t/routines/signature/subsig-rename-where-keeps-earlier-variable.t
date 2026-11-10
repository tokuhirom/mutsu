use Test;

plan 9;

# The key of a named rename in a sub-signature (`left` of `:left($l)`) is no
# variable: a `where` on it must see the `$left` an earlier parameter bound.
class Ge { has $.left; has $.right }
my $p = Ge.new(left => 'a', right => 1);
my $q = Ge.new(left => 'b', right => 5);

sub bound(Ge (:$left, |), Ge (:left($l) where { $left eq $_ }, |)) { "match" }
is (try { bound($p, $p) }), 'match', 'same left: the clause holds';
nok (try { bound($p, $q) }).defined, 'different left: the clause fails';

multi pick-one(Ge (:$left, |), Ge (:left($l) where { $left eq $_ }, |)) { "two" }
multi pick-one(|) { "fallback" }
is pick-one($p, $p), 'two', 'multi: the clause selects the candidate';
is pick-one($p, $q), 'fallback', 'multi: the clause rejects it';

class O {
    multi method opt(Ge (:$left, |), Ge (:left($l2) where { $left eq $_ }, |), 1) {
        nextsame unless $left eq 'a';
        "between"
    }
    multi method opt(Ge $x (:$left, |), Ge $y (:left($l2) where { $left eq $_ }, |), $) { "second" }
    multi method opt(|) { "fallback" }
}
is O.opt($p, $p, 1), 'between', 'method multi: first candidate';
is O.opt($p, $p, 2), 'second', 'method multi: second candidate';
is O.opt($p, $q, 1), 'fallback', 'method multi: nextsame past a rejecting where';
is O.opt($p, $q, 2), 'fallback', 'method multi: where rejects';

sub own(Ge (:left($l) where { $_ eq 'a' }, |)) { $l }
is own($p), 'a', 'a where on the rename itself still sees the value as $_';
