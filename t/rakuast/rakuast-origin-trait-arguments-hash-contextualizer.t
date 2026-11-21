use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: the colonpairs inside
# `%( ... )` are placed as a hash composer's, and a routine trait's
# parenthesized argument is placed with what it holds.

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
is placed(Q[my %h = %(:a(1), :b<c>)]),
    'VarDeclaration::Simple@0-23 Initializer::Assign@6-23 Contextualizer::Hash@8-23 ApplyListInfix@10-22 Infix@15-16 ColonPair::Value@10-15 Circumfix::Parentheses@12-15 SemiList@13-14 IntLiteral@13-14 ColonPair::Value@17-22 QuotedString@19-22 StrLiteral@20-21',
    'colonpairs in a hash contextualizer';
is placed(Q[sub f() is DEPRECATED("x") { 1 }]),
    'Sub@0-32 Name@4-5 Signature@6-6 Trait::Is@8-26 Name@11-21 Circumfix::Parentheses@21-26 SemiList@22-25 QuotedString@22-25 StrLiteral@23-24 Blockoid@27-32 IntLiteral@29-30',
    'a routine trait with a string argument';
is placed(Q[sub g() is export(:DEFAULT) { 1 }]),
    'Sub@0-33 Name@4-5 Signature@6-6 Trait::Is@8-27 Name@11-17 Circumfix::Parentheses@17-27 SemiList@18-26 ColonPair::True@18-26 Blockoid@28-33 IntLiteral@30-31',
    'a routine trait with a colonpair argument';
