use Test;

plan 15;

sub expr($source) { $source.AST.statements.head.expression }

for Q[my submethod local-name($x) { $x + 1 }],
    Q[our submethod local-name($x) { $x + 1 }],
    Q[submethod local-name($x) { $x + 1 }] -> $source {
    my $node = expr($source);
    isa-ok $node, RakuAST::Submethod, 'a named submethod is a Submethod';
    is $node.name.gist, 'RakuAST::Name.from-identifier("local-name")',
        'the declared name is retained';
    is $node.signature.parameters.elems, 1, 'the written parameter is retained';
    unlike $node.gist, /scope/, 'Rakudo omits scope from Submethod';
}

is EVAL(Q[my submethod round-trip($x) { $x + 1 }; &round-trip(Any, 4)].AST),
    5, 'a my submethod round-trips and remains callable';
is EVAL(Q[our submethod round-trip($x) { $x + 1 }; &round-trip(Any, 5)].AST),
    6, 'an our submethod round-trips and remains callable';
is EVAL(Q[submethod round-trip($x) { $x + 1 }; &round-trip(Any, 6)].AST),
    7, 'an unscoped submethod round-trips and remains callable';
