use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: the adverbs written after an
# infix application (`1 bar 2 :!foo :bar(3)`) are placed and the application
# runs to their end; an indirect name `::("Int")` spans its term.

plan 3;
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
is placed(Q[sub infix:<bar>($a, $b, *%n) { $a }; my $x = 1 bar 2 :!foo :bar(3)]),
    'Sub@0-35 Name@4-15 QuotedString@10-15 StrLiteral@11-14 Signature@16-27 Parameter@16-18 ParameterTarget::Var@16-18 VarDeclaration::Simple@16-18 Parameter@20-22 ParameterTarget::Var@20-22 VarDeclaration::Simple@20-22 Parameter@24-27 ParameterTarget::Var@25-27 VarDeclaration::Simple@25-27 Blockoid@29-35 Var::Lexical@31-33 VarDeclaration::Simple@37-66 Initializer::Assign@43-66 ApplyInfix@45-66 Infix@47-50 ArgList@45-66 IntLiteral@45-46 IntLiteral@51-52 ColonPair::False@53-58 ColonPair::Value@59-66 Circumfix::Parentheses@63-66 SemiList@64-65 IntLiteral@64-65',
    'adverbs after an infix application';
is placed(Q[::("Int")]),
    'Term::Name@0-9 Name@0-9 QuotedString@3-8 StrLiteral@4-7',
    'an indirect name';
is placed(Q[say ::(q{Int}).^name]),
    'Call::Name::WithoutParentheses@0-20 Name@0-3 ArgList@4-20 ApplyPostfix@4-20 Term::Name@4-14 Name@4-14 QuotedString@7-13 StrLiteral@9-12 Call::MetaMethod@16-20 ArgList@20-20',
    'an indirect name as a call argument';
