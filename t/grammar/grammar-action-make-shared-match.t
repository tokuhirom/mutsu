use Test;

# `make` writes the `.made` of the one Match object, as in rakudo (#12954).
# A Match kept before its action ran (a code block keeping `$<num>`, an
# action keeping its `$/`) observes the later `make`, and it is the very
# Match the finished tree files.

plan 13;

{
    my @kept;
    grammar G { token TOP { <num> { @kept.push: $<num> } }; token num { \d+ } }
    class A { method num($/) { make +$/ * 2 } }
    my $m = G.parse('5', :actions(A));
    is $m<num>.made, 10, 'the tree carries the action .made';
    is @kept[0].made, 10, 'a Match kept by a code block sees the later make';
    ok @kept[0] === $m<num>, 'the kept Match is the tree node';
}

{
    my @kept;
    grammar Q { token TOP { [<num> { @kept.push: $<num>[*-1] }]+ % ',' }; token num { \d+ } }
    class QA { method num($/) { make $/.Int + 100 } }
    my $m = Q.parse('1,2,3', :actions(QA));
    is @kept.map(*.made).join(','), '101,102,103', 'kept quantified captures see their .made';
    is $m<num>.map(*.made).join(','), '101,102,103', 'the tree agrees';
}

{
    my $kept;
    grammar K { token TOP { <num> }; token num { \d+ } }
    class KA {
        method num($/) { $kept = $/; make 42 }
        method TOP($/) { make $<num>.made + 1 }
    }
    my $m = K.parse('9', :actions(KA));
    is $kept.made, 42, 'an action that keeps its $/ sees its own make';
    is $m.made, 43, 'the parent action reads the child .made';
    ok $kept === $m<num>, 'the action $/ is the tree node';
}

{
    grammar Al { token TOP { <x=num> }; token num { \d+ } }
    class AlA { method num($/) { make 7 } }
    is Al.parse('3', :actions(AlA))<x>.made, 7, 'an aliased capture carries its .made';
}

{
    grammar P { token TOP { (<num>)+ }; token num { \d } }
    class PA { method num($/) { make +$/ * 3 } }
    my $m = P.parse('12', :actions(PA));
    is $m[0].map({ .<num>.made }).join(','), '3,6', 'subrules inside a positional group';
}

{
    'abc' ~~ /b/;
    make 5;
    is $/.made, 5, 'make outside a grammar sets $/.made';
    my $m = 'xyz' ~~ /y/;
    my $alias = $m;
    $m.make(6);
    is $m.made, 6, 'Match.make sets .made';
    is $alias.made, 6, 'another holder of the Match sees it';
}
