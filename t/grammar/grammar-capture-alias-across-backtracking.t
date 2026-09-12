use Test;

# A non-suppressing alias `<val=word>` files its match under BOTH names, and
# the engine records the alias in a capture-alias map that every candidate
# clones, merges and rewinds. Putting the alias inside an alternation under a
# separated quantifier makes the matcher build and undo those records several
# times per item before the parse commits, so an alias that leaked across a
# rewind — or one merged under the wrong name — shows up here.
plan 12;

grammar G {
    token TOP { <item>+ %% ',' }
    token item { [ <val=word> '!' | <val=digits> ';' ] }
    token word { \w+ }
    token digits { \d+ }
}

my $m = G.parse('abc!,12;,x!');
ok $m.defined, 'the alternation parse succeeds';
is $m<item>.elems, 3, 'every separated item matched';

is ~$m<item>[0]<val>, 'abc', 'the first item captures under the alias';
is ~$m<item>[0]<word>, 'abc', 'and under the aliased rule name';
nok $m<item>[0]<digits>.defined, 'the rejected branch leaves no capture behind';

is ~$m<item>[1]<val>, '12', 'the second item captures under the alias';
is ~$m<item>[1]<digits>, '12', 'and under its own branch rule name';
nok $m<item>[1]<word>.defined, 'the branch that backtracked left nothing';

is ~$m<item>[2]<word>, 'x', 'the third item is unaffected by the two before it';

nok G.parse('abc?').defined, 'a parse that fails overall still fails';

# A quantified alias collects the whole list under both names.
grammar H {
    token TOP { <n=num>+ }
    token num { \d }
}
my $h = H.parse('123');
is $h<n>.map(~*).join(','), '1,2,3', 'a quantified alias collects every match';
is $h<num>.map(~*).join(','), '1,2,3', 'and so does the aliased rule name';
