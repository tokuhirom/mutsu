use Test;

# A Junction collapses to a Bool through each eigenstate's own `Bool` method,
# short-circuiting where rakudo stops asking (#17513: Red's query ASTs explore
# their branches from that `Bool`, so a `grep({ !($_ eq any(...)) })` filter
# compiled to `WHERE 0`).

my @asked;
class C {
    has $.n;
    has $.truth;
    method Bool { @asked.push: $!n; $!truth }
}

sub j(&kind, *@truths) { kind(|@truths.kv.map(-> $n, $t { C.new(:$n, :truth($t)) })) }

plan 22;

@asked = ();
is-deeply ?j(&any, False, False), False, 'any of two False eigenstates is False';
is-deeply @asked, [0, 1], 'any asks every eigenstate when none is True';

@asked = ();
is-deeply ?j(&any, True, False), True, 'any with a True eigenstate is True';
is-deeply @asked, [0], 'any stops at the first True';

@asked = ();
is-deeply ?j(&all, True, False, True), False, 'all with a False eigenstate is False';
is-deeply @asked, [0, 1], 'all stops at the first False';

@asked = ();
is-deeply ?j(&one, True, True, False), False, 'one with two True eigenstates is False';
is-deeply @asked, [0, 1], 'one stops at the second True';

is-deeply ?j(&one, False, True), True, 'one with exactly one True is True';

@asked = ();
is-deeply ?j(&none, False, True, False), False, 'none with a True eigenstate is False';
is-deeply @asked, [0, 1], 'none stops at the first True';

my $j = j(&any, False, False);
is-deeply !$j, True, 'prefix ! negates the collapsed junction';
is-deeply (so $j), False, 'so';
is-deeply (not $j), True, 'not';
is-deeply $j.Bool, False, '.Bool';
is-deeply $j.so, False, '.so';
is-deeply $j.not, True, '.not';
is (if $j { 'yes' } else { 'no' }), 'no', 'if collapses through the user Bool';

is-deeply ?j(&all, True, True), True, 'all of True eigenstates';
is-deeply ?any(0, ''), False, 'plain values still collapse by their truth';
is-deeply all(1, 'a').so, True, 'plain all';
is-deeply none(0, '').not, False, 'plain none negated';
