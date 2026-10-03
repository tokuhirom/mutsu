use Test;

plan 5;

# A user `multi sub postcircumfix:<[; ]>` / `<{; }>` receives a multi-dim
# subscript on a matching object, with the indices as one list.
class M {
    method value-at($i, $j) { "v($i,$j)" }
    method row-slice(*@i) { "rows:" ~ @i.join(',') }
}
multi sub postcircumfix:<[; ]>(M:D $mat, @indexes) {
    given (@indexes[0], @indexes[1]) {
        when $_.head ~~ Int && $_.tail ~~ Int { $mat.value-at(@indexes[0], @indexes[1]) }
        when $_.head ~~ Iterable { $mat.row-slice($_.head) }
        default { 'other' }
    }
}
multi sub postcircumfix:<{; }>(M:D $mat, @keys) { 'keys:' ~ @keys.join('|') }

my $m = M.new;
is $m[3;2], 'v(3,2)', '[;] subscript reaches the user operator';
is $m[<b c>; <A C>], 'rows:b,c', 'a List index is passed unflattened, and flattens into a slurpy';
is $m{'a';'b'}, 'keys:a|b', '{;} subscript reaches the user operator';
is-deeply ($m[1;2], 'x'), ('v(1,2)', 'x'), 'also as an element of a list';
sub take-arg($x) { $x }
is take-arg($m[4;5]), 'v(4,5)', 'also as a call argument';
