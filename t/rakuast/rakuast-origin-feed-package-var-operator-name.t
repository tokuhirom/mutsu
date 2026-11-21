use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a chained feed places its
# `Feed` at the last arrow; a package variable's `Name` runs after its
# sigil; an operator called by name keeps its symbol as the word quote of
# its `Name`, placed between the angle brackets.

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
is placed(Q[my @a = 1, 2, 3; @a ==> map({ $_ * 2 }) ==> my @out]),
    'VarDeclaration::Simple@0-15 Initializer::Assign@6-15 ApplyListInfix@8-15 Infix@12-13 IntLiteral@8-9 IntLiteral@11-12 IntLiteral@14-15 ApplyListInfix@17-51 Feed@40-43 Var::Lexical@17-19 Call::Name@24-39 Name@24-27 ArgList@28-38 Block@28-38 Blockoid@28-38 ApplyInfix@30-36 Infix@33-34 ArgList@30-36 Var::Lexical@30-32 IntLiteral@35-36 VarDeclaration::Simple@44-51',
    'a chained feed';
is placed(Q[my $n = "v"; my $v = 4; say $::($n)]),
    'VarDeclaration::Simple@0-11 Initializer::Assign@6-11 QuotedString@8-11 StrLiteral@9-10 VarDeclaration::Simple@13-22 Initializer::Assign@19-22 IntLiteral@21-22 Call::Name::WithoutParentheses@24-35 Name@24-27 ArgList@28-35 Var::Package@28-35 Name@29-35 Var::Lexical@32-34',
    'an indirect package variable';
is placed(Q[say $OUR::x]),
    'Call::Name::WithoutParentheses@0-11 Name@0-3 ArgList@4-11 Var::Package@4-11 Name@5-11',
    'a qualified package variable';
is placed(Q[say infix:<+>(1, 2)]),
    'Call::Name::WithoutParentheses@0-19 Name@0-3 ArgList@4-19 Call::Name@4-19 Name@4-13 QuotedString@10-13 StrLiteral@11-12 ArgList@14-18 IntLiteral@14-15 IntLiteral@17-18',
    'an infix operator called by name';
is placed(Q[say postfix:<++>(my $x = 1)]),
    'Call::Name::WithoutParentheses@0-27 Name@0-3 ArgList@4-27 Call::Name@4-27 Name@4-16 QuotedString@12-16 StrLiteral@13-15 ArgList@17-26 VarDeclaration::Simple@17-26 Initializer::Assign@23-26 IntLiteral@25-26',
    'a postfix operator called by name';
