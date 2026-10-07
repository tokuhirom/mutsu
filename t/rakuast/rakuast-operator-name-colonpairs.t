use Test;

# A declared operator name in RakuAST, measured on rakudo 2026.09: the
# category is the identifier and the symbol is a word-list QuotedString in
# `colonpairs`, i.e. `sub infix:<foo>` is named
# `Name.from-identifier("infix", colonpairs => (QuotedString<words val>("foo"),))`.

plan 15;

for 'infix:<foo>', 'prefix:<X>', 'postfix:<ii>', 'circumfix:<[ ]>' -> $decl {
    my ($category, $symbol) = $decl.match(/^ (\w+) ':<' (.*) '>' $/).list;
    my $name = ("sub " ~ $decl ~ q[($a) { }]).AST.statements.head.expression.name;
    isa-ok $name, RakuAST::Name, "`$decl` has a Name";
    my $pairs = $name.colonpairs;
    is $name.parts.map(*.name).join('::'), ~$category, "`$decl`: the category is the identifier";
    is $pairs.head.segments.head.value, ~$symbol, "`$decl`: the symbol is the adverb text";
}

my $gist = Q[sub infix:<foo>($a, $b) { }].AST.gist;
ok $gist.contains(
    'name      => RakuAST::Name.from-identifier("infix", colonpairs => ('),
    'the gist spells the name with colonpairs';
is EVAL(Q[sub infix:<bar>($a, $b) { $a ~ "-" ~ $b }; 1 bar 2].AST),
    '1-2', 'an infix declared from its AST works';
is EVAL(Q[multi sub prefix:<X>($a) { -$a }; X 3].AST),
    -3, 'so does a multi prefix';

done-testing;
