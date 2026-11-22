use Test;

# A Seq `:nth` (`1…3`) selects matches as a List does, for `.match`, `.subst`
# and `subst-mutate`, and when nothing matches a list-valued `:nth` answers the
# empty list (only a single index answers Nil) (roast
# 6.c/APPENDICES/A03-older-specs/01-misc.t).

plan 8;

is-deeply "abb".match(/b/, :nth(1…3))».Str, ('b', 'b'), '.match :nth(Seq)';
is "abb".subst(/b/, '', :nth(1…3)), 'a', '.subst :nth(Seq)';
my $s = 'abb';
isa-ok $s.subst-mutate(:nth(1…3), 'b', ''), List, 'subst-mutate :nth(Seq) with a Str matcher answers a List';
is-deeply "xyz".match(/b/, :nth(1, 2)), (), 'no match, :nth(List): empty list';
is-deeply "xyz".match(/b/, :nth(1..2)), (), 'no match, :nth(Range): empty list';
is-deeply "xyz".match(/b/, :nth(1)), Nil, 'no match, :nth(Int): Nil';
my $n = 123;
is-deeply $n.subst-mutate(:nth(1, 2, 3), /b/, ''), (), 'Cool subst-mutate, no match, :nth(List)';
my $t = 'aaa';
is-deeply $t.subst-mutate(:nth(1…3), 'b', ''), (), 'Str matcher, no match, :nth(Seq)';
