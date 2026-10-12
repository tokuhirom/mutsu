use Test;

plan 8;

sub attr(Str $src) {
    $src.AST.statements[0].expression.body.body.statement-list.statements[0].expression
}

{
    my $a = attr(Q[class A1 { has $.book handles ('title', 'author', year => 'publication') }]);
    is $a.traits.elems, 1, 'handles remains one attribute trait';
    isa-ok $a.traits[0], RakuAST::Trait::Handles, 'the delegation trait class';
    like $a.traits[0].gist, /'Circumfix::Parentheses' .* 'FatArrow'/,
        'quoted names and rename pairs keep their written term';
}

{
    my $a = attr(Q[class A2 { has $.book handles /<[a..z]>+ '-' bar/ }]);
    isa-ok $a.traits[0], RakuAST::Trait::Handles,
        'a regex delegation keeps a Trait::Handles node';
}

{
    my $a = attr(Q[class A3 { has $.delegate handles ('fn', |A3.names) }]);
    isa-ok $a.traits[0], RakuAST::Trait::Handles,
        'a mixed dynamic delegation list keeps its source term';
}

is EVAL(Q[class B1 { has $.book handles ('alias' => 'title') }; class T1 { method title { 42 } }; B1.new(book => T1.new).alias].AST),
    42, 'a quoted rename delegates after round trip';

is EVAL(Q[class B2 { has $.book handles /<[a..z]>+/ }; class T2 { method title { 7 } }; B2.new(book => T2.new).title].AST),
    7, 'a regex handles term delegates after round trip';

is EVAL(Q[class H1 { method names { <given family> } }; class D1 { method given { 1 }; method family { 2 } }; class C1 { has $.d handles ("fn", |H1.names) }; C1.new(d => D1.new).family].AST),
    2, 'a dynamic list term delegates after round trip';
