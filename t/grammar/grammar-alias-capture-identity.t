use v6;
use Test;

# A non-suppressing alias `<val=word>` files ONE capture under BOTH names, so
# `$m<val>` and `$m<word>` are the same `Match` object -- `===` on them is True
# in rakudo, and `.WHICH` agrees.
#
# The capture store already did that: #7576 round 10 made such an alias store a
# single `Arc<CapNode>` under both slots. The identity was lost on the way OUT,
# in the lazy-Match materialization, which minted a `MatchNode` (and so a fresh
# instance id) per SLOT rather than per node. The action-driven path builds its
# Match objects through the reduce walk and kept the sharing, so the bug showed
# only on an action-less `.parse` -- the two paths disagreed about whether an
# aliased capture is one object or two (#8167).
#
# Every expectation below was measured against rakudo.

plan 15;

grammar A { token TOP { <val=word> }; token word { \w+ } }
class Act { }

# The reported case: action-less and action-driven parses must agree.
{
    my $m = A.parse('abc');
    ok $m<val> === $m<word>, 'an alias hands out ONE Match for both names';
    ok $m<val>.WHICH eq $m<word>.WHICH, 'and their .WHICH agrees';
    ok $m<val> eqv $m<word>, 'eqv agrees too (it always did)';
    is $m.hash.keys.sort.join(','), 'val,word', 'both names are in .hash';
    is $m<val>.Str, 'abc', 'the shared Match still carries its own span';

    my $n = A.parse('abc', :actions(Act));
    ok $n<val> === $n<word>, 'the action-driven path agrees (it always did)';
}

# Reading the same name twice was never broken; pin it so the memo cannot
# regress in the other direction.
{
    my $m = A.parse('abc');
    ok $m<word> === $m<word>, 'two reads of one name are the same object';
    ok $m.hash<val> === $m.hash<word>, '.hash exposes the shared object too';
}

# The alias is shared wherever it appears, not only at the top level.
{
    grammar B { token TOP { [ <val=word> ] }; token word { \w+ } }
    my $b = B.parse('xy');
    ok $b<val> === $b<word>, 'inside a non-capturing group';

    grammar C { token TOP { <val=word> | <num=digits> }; token word { \w+ }; token digits { \d+ } }
    my $c = C.parse('zz');
    ok $c<val> === $c<word>, 'inside an alternation';

    grammar D { token TOP { <item> }; token item { <val=word> }; token word { \w+ } }
    my $d = D.parse('abc');
    ok $d<item><val> === $d<item><word>, 'through a nested subrule';
}

# A quantified alias shares per iteration.
{
    grammar F { token TOP { <val=word>+ }; token word { \w } }
    my $f = F.parse('ab');
    is $f<val>.elems, 2, 'a quantified alias captures each iteration';
    ok $f<val>[0] === $f<word>[0], 'and shares the Match per iteration';
}

# A SUPPRESSING alias `<val=.word>` files one name only -- the sharing must not
# invent the second.
{
    grammar E { token TOP { <val=.word> }; token word { \w+ } }
    my $e = E.parse('q');
    is $e.hash.keys.sort.join(','), 'val', 'a suppressing alias files one name';
}

# An ordinary positional capture is unaffected.
{
    my $g = 'abc' ~~ / (\w) (\w) /;
    ok $g[0] === $g[0], 'a positional capture keeps a stable identity';
}

done-testing;
