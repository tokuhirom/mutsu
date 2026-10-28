use Test;

# The eager warm-up prefix of an unbounded closure sequence (a) stops when
# elements get huge even when they are Rats or lists, and (b) snapshots a
# returned array, since a generator returning a container it keeps mutating
# (`state @row`) hands back the same array on every step. Found with
# Math::Sequences (A001699 and A005811). #15350

plan 3;

my @rats = flat 1, 1, -> $a, $b { $b * ($a + $b + $b / $a) }...*;
is-deeply @rats[^6].map(*.Int).List, (1, 1, 3, 21, 651, 457653), 'Rat elements that grow doubly exponentially do not hang';

my @rows = 0, 1, 2, 1, {
    state @row = 2, 1;
    @row = flat @row[^(@row.elems div 2)], @row[^(@row.elems div 2)].reverse.map(* + 1), @row;
}...*;
is-deeply @rows[4].List, (2, 3, 2, 1), 'the first generated row is not the last one (no aliasing of the state array)';

my @flat = lazy flat 0, 1, 2, 1, {
    state @row = 2, 1;
    @row = flat @row[^(@row.elems div 2)], @row[^(@row.elems div 2)].reverse.map(* + 1), @row;
}...*;
is-deeply @flat[^8].List, (0, 1, 2, 1, 2, 3, 2, 1), 'flat over the rows reads each row as it was generated';
