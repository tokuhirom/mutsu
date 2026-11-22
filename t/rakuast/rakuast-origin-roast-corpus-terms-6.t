use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11, for a sixth set of
# constructs the roast corpus showed unplaced. A sigilless `\term:<...>` and
# a package-qualified `M::term:<...>` carry their `<...>` as a colonpair of
# the `Name`, as in Rakudo.

plan 8;
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
is placed(Q[say ${ a => 1, b => 2 }]),
    'Call::Name::WithoutParentheses@0-23 Name@0-3 ArgList@4-23 Contextualizer::Item@4-23 Circumfix::HashComposer@5-23 ApplyListInfix@7-21 Infix@13-14 FatArrow@7-13 IntLiteral@12-13 FatArrow@15-21 IntLiteral@20-21',
    'a hash composer right after the item sigil';
is placed(Q[my \term:<ℵ₀> = 5; say ℵ₀]),
    'VarDeclaration::Term@0-17 Name@4-13 QuotedString@9-13 StrLiteral@10-12 Initializer::Assign@14-17 IntLiteral@16-17 Call::Name::WithoutParentheses@19-25 Name@19-22 ArgList@23-25 Term::Name@23-25',
    'a sigilless term declared under its operator name';
is placed(Q[say 1 X~ 2 X~ 3]),
    'Call::Name::WithoutParentheses@0-15 Name@0-3 ArgList@4-15 ApplyListInfix@4-15 MetaInfix::Cross@11-13 Infix@12-13 IntLiteral@4-5 IntLiteral@9-10 IntLiteral@14-15',
    'a cross metaop list, on its last operator';
is placed(Q[say (1 Z+ 2)]),
    'Call::Name::WithoutParentheses@0-12 Name@0-3 ArgList@4-12 Circumfix::Parentheses@4-12 SemiList@5-11 ApplyListInfix@5-11 MetaInfix::Zip@7-9 Infix@8-9 IntLiteral@5-6 IntLiteral@10-11',
    'a zip metaop in parentheses';
is placed(Q[say .(1+2) * 30]),
    'Call::Name::WithoutParentheses@0-15 Name@0-3 ArgList@4-15 ApplyInfix@4-15 Infix@11-12 ArgList@4-15 Term::TopicCall@4-10 Call::Term@5-10 ArgList@6-9 ApplyInfix@6-9 Infix@7-8 ArgList@6-9 IntLiteral@6-7 IntLiteral@8-9 IntLiteral@13-15',
    'a call on the topic';
is placed(Q[my $t:foo<a b>:bar= "D"]),
    'VarDeclaration::Simple@0-23 Initializer::Assign@18-23 QuotedString@20-23 StrLiteral@21-22',
    'an initializer right after a name adverb';
is placed(Q[module Foo { our sub term:<ab> { 1 } }; say Foo::term:<ab>]),
    'Module@0-38 Name@7-10 Block@11-38 Blockoid@11-38 Sub@13-36 Name@21-30 QuotedString@26-30 StrLiteral@27-29 Blockoid@31-36 IntLiteral@33-34 Call::Name::WithoutParentheses@40-58 Name@40-43 ArgList@44-58 Call::Name@44-58 Name@44-58 QuotedString@54-58 StrLiteral@55-57 ArgList@58-58',
    'a package-qualified operator name';
is placed(Q[say (class Q { }).^name]),
    'Call::Name::WithoutParentheses@0-23 Name@0-3 ArgList@4-23 ApplyPostfix@4-23 Class@5-16 Name@11-12 Block@13-16 Blockoid@13-16 Call::MetaMethod@19-23 ArgList@23-23',
    'a parenthesized class as an operand';
