use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: the colonpairs of a hash
# composer (`:2b`, `:!c`, `:d<x y>`, `:e(5)`, `:z`) are placed with their
# values, and `:key[...]` places its brackets. A `token`'s signature may
# follow its name after a blank.

plan 7;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) unless $class ~~ /Statement/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[{:2b, :!c, :d<x y>, :e(5), :z}]),
    'Circumfix::HashComposer@0-30 ApplyListInfix@1-29 Infix@25-26 ColonPair::Number@1-4 IntLiteral@2-3 ColonPair::False@6-9 ColonPair::Value@11-18 QuotedString@13-18 StrLiteral@14-17 ColonPair::Value@20-25 Circumfix::Parentheses@22-25 SemiList@23-24 IntLiteral@23-24 ColonPair::True@27-29',
    'every colonpair form, comma-separated';
is placed(Q[{:e(5)}]),
    'Circumfix::HashComposer@0-7 ColonPair::Value@1-6 Circumfix::Parentheses@3-6 SemiList@4-5 IntLiteral@4-5',
    'a lone colonpair';
is placed(Q[{ "k" => 1, :x<a b>, :!y }]),
    'Circumfix::HashComposer@0-26 ApplyListInfix@2-24 Infix@19-20 ApplyInfix@2-10 Infix@6-8 ArgList@2-10 QuotedString@2-5 StrLiteral@3-4 IntLiteral@9-10 ColonPair::Value@12-19 QuotedString@14-19 StrLiteral@15-18 ColonPair::False@21-24',
    'colonpairs after a placed pair';
is placed(Q[{:h{ a => 1 }}]),
    'Circumfix::HashComposer@0-14 ColonPair::Value@1-13 Circumfix::HashComposer@3-13 FatArrow@5-11 IntLiteral@10-11',
    'a hash composer as a value';
is placed(Q[{:i('a)b'), :j(":k(") }]),
    'Circumfix::HashComposer@0-23 ApplyListInfix@1-21 Infix@10-11 ColonPair::Value@1-10 Circumfix::Parentheses@3-10 SemiList@4-9 QuotedString@4-9 StrLiteral@5-8 ColonPair::Value@12-21 Circumfix::Parentheses@14-21 SemiList@15-20 QuotedString@15-20 StrLiteral@16-19',
    'brackets and quotes inside the values';
is placed(Q[say(:g[1, 2])]),
    'Call::Name@0-13 Name@0-3 ArgList@4-12 ColonPair::Value@4-12 Circumfix::ArrayComposer@6-12 SemiList@7-11 ApplyListInfix@7-11 Infix@8-9 IntLiteral@7-8 IntLiteral@10-11',
    'a bracketed value in a call';
is placed(Q[grammar G { token word ($e) { a } }]),
    'Grammar@0-35 Name@8-9 Block@10-35 Blockoid@10-35 TokenDeclaration@12-33 Name@18-22 Signature@24-26 Parameter@24-26 ParameterTarget::Var@24-26 VarDeclaration::Simple@24-26 Regex::WithWhitespace@30-32 Regex::Literal@30-31',
    'a token signature after a blank';
