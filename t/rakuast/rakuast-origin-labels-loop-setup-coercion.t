use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a statement visits its
# labels last, each placed over its `NAME:`, and a label named as an
# argument (`last L`) is a placed `Term::Name`; a C-style loop's setup is
# placed; `Int:D(Str)` places its inner `Type::Simple` over the whole
# coercion; a sigilless term shadowing a setting type (`my \uint8`) is a
# `Term::Name` in its block.

plan 5;
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
is placed(Q[L: for 1..2 { last L }]),
    'ApplyInfix@7-11 Infix@8-10 ArgList@7-11 IntLiteral@7-8 IntLiteral@10-11 Block@12-22 Blockoid@12-22 Call::Name::WithoutParentheses@14-20 Name@14-18 ArgList@19-20 Term::Name@19-20 Name@19-20 Label@0-2',
    'a labelled loop visits its label last';
is placed(Q[R: say 1]),
    'Call::Name::WithoutParentheses@3-8 Name@3-6 ArgList@7-8 IntLiteral@7-8 Label@0-2',
    'a labelled expression statement';
is placed(Q[Q: loop (my $i = 0; $i < 1; $i++) { next Q }]),
    'VarDeclaration::Simple@9-18 Initializer::Assign@15-18 IntLiteral@17-18 ApplyInfix@20-26 Infix@23-24 ArgList@20-26 Var::Lexical@20-22 IntLiteral@25-26 ApplyPostfix@28-32 Var::Lexical@28-30 Postfix@30-32 Block@34-44 Blockoid@34-44 Call::Name::WithoutParentheses@36-42 Name@36-40 ArgList@41-42 Term::Name@41-42 Name@41-42 Label@0-2',
    'a labelled C-style loop with a setup';
is placed(Q[my Int:D(Str) $dir = "3";]),
    'VarDeclaration::Simple@0-24 Type::Coercion@3-13 Type::Simple@3-13 Name@3-6 Type::Simple@9-12 Name@9-12 Initializer::Assign@19-24 QuotedString@21-24 StrLiteral@22-23',
    'a coercion over a definite type';
is placed(Q[{ my \uint8 = 3; say uint8 }]),
    'Block@0-28 Blockoid@0-28 VarDeclaration::Term@2-15 Name@6-11 Initializer::Assign@12-15 IntLiteral@14-15 Call::Name::WithoutParentheses@17-26 Name@17-20 ArgList@21-26 Term::Name@21-26 Name@21-26',
    'a sigilless term shadowing a native type';
