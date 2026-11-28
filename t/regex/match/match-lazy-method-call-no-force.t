# A method call on a Match reads its captures without materializing the
# Match (#12917): `$/.values`, `$/.list`, `$/.hash`, `$/[0]` and `$<name>`
# answer from the capture node, sharing one memoized list and hash. These pin
# that every read agrees, that repeated reads hand out the same child Match,
# and that code blocks in a grammar see the same values. Values are rakudo's.
use Test;

plan 18;

my $m = 'ab12' ~~ / (\w) (\w) $<num>=[\d+] /;
is $m.values.elems, 3, '.values has the positional and named captures';
is $m.list.map(~*).join(','), 'a,b', '.list';
is ~$m.hash<num>, '12', '.hash';
is ~$m[1], 'b', 'a subscript';
ok $m[0] === $m.list[0], 'the subscript and .list hand out the same child';
ok $m[0] === $m[0], 'a repeated subscript hands out the same child';
ok $m<num> === $m.hash<num>, 'the named read and .hash agree';
is $m.Str, 'ab12', 'and the Match still stringifies';
is $m.^name, 'Match', 'and is a Match';

grammar G {
    token TOP { <plain> { make $/.values[0].ast } }
    token plain { $<value>=.* { make ~$<value> } }
}
is G.parse('value 1').made, 'value 1', '.values[0].ast in a code block';

grammar H {
    token TOP { (\d) (\d) { make $/.list.map(+*).sum } }
}
is H.parse('34').made, 7, '.list in a code block';

grammar N {
    token TOP { <a=.alpha> <b=.alpha> { make $/.hash.keys.sort.join } }
}
is N.parse('xy').made, 'ab', '.hash in a code block';

grammar K {
    token TOP { (\d+) { make $0 + 1 } }
}
is K.parse('41').made, 42, '$0 in a code block';

# A Match as a bare statement is sunk without effect.
{
    my $r = 'q' ~~ /q/;
    sink $r;
    'z' ~~ /z/;
    pass 'sinking a Match does nothing';
}

# A Match after it was materialized by a forcing read still agrees.
{
    my $x = 'cd' ~~ / (\w) (\w) /;
    my $first = $x[0];
    my $dump = $x.raku;
    ok $x[0] === $first, 'the child is the same after a materializing read';
    is $x.list.elems, 2, 'and the list is unchanged';
}

# A grammar class's own method on a Match still wins.
grammar Own {
    token TOP { \w+ }
    method values { 'own' }
}
is Own.parse('abc').values, 'own', 'a grammar method named values is called';

# make on a child read through .list is seen through the subscript.
{
    my $y = 'ef' ~~ / (\w) (\w) /;
    $y.list[1].make('made');
    is $y[1].made, 'made', 'make through .list is seen through the subscript';
}
