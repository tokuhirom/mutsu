use Test;

plan 16;

# A class that does Sequence and Positional with only its own `iterator`
# (Red's ResultSeq) is subscripted and listed through its elements, plain or
# with a role mixed in.
class It does Iterator {
    has $.n = 0;
    method pull-one { $!n < 6 ?? ($!n++) !! IterationEnd }
}
class S does Sequence does Positional { method iterator { It.new } }
role R { }

for 'plain' => S.new, 'mixin' => (S.new but R) -> $case {
    my $s = $case.value;
    my $k = $case.key;
    is $s[0], 0, "$k: [0]";
    is $s[3], 3, "$k: [3]";
    is $s[*-1], 5, "$k: [*-1]";
    is-deeply $s[1, 2].list, (1, 2).list, "$k: a slice";
    ok so($s.any > 4), "$k: any junction";
    ok so($s.one == 3), "$k: one junction";
    is $s.pairs.elems, 6, "$k: pairs";
    nok $s[10].defined, "$k: a missing element";
}
