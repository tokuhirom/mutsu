use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a pointy block's
# parameters place their `is NAME` traits; the parameters of a statement's
# pointy block (`given`, `while`, `if`) are placed; a colonpair among the
# words of `« ... »` is a placed `QuoteWordsAtom`; and a typed enum places its
# `of` type, visited after its term.

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
is placed(Q[my $f = -> Int $y is copy is rw { 1 }]),
    'VarDeclaration::Simple@0-37 Initializer::Assign@6-37 PointyBlock@8-37 Signature@11-31 Parameter@11-31 Type::Simple@11-14 Name@11-14 ParameterTarget::Var@15-17 VarDeclaration::Simple@15-17 Type::Simple@11-14 Name@11-14 Trait::Is@18-25 Name@21-25 Trait::Is@26-31 Name@29-31 Blockoid@32-37 IntLiteral@34-35',
    'parameter traits in a pointy block';
is placed(Q[given 1 -> $_ is copy { 1 }]),
    'IntLiteral@6-7 PointyBlock@8-27 Signature@11-21 Parameter@11-21 ParameterTarget::Var@11-13 VarDeclaration::Simple@11-13 Trait::Is@14-21 Name@17-21 Blockoid@22-27 IntLiteral@24-25',
    'a given block parameter with a trait';
is placed(Q[while 0 -> $y { 1 }]),
    'IntLiteral@6-7 PointyBlock@8-19 Signature@11-13 Parameter@11-13 ParameterTarget::Var@11-13 VarDeclaration::Simple@11-13 Blockoid@14-19 IntLiteral@16-17',
    'a while block parameter';
is placed(Q[my @a = « :X<x> b »]),
    'VarDeclaration::Simple@0-19 Initializer::Assign@6-19 QuotedString@8-19 StrLiteral@9-10 QuoteWordsAtom@10-15 ColonPair::Value@10-15 QuotedString@12-15 StrLiteral@13-14 StrLiteral@15-18',
    'a colonpair among interpolated words';
is placed(Q[our Str enum Sel « :Alpha<a-val> :Beta<b> »]),
    'Type::Enum@0-43 Name@13-16 QuotedString@17-43 StrLiteral@18-19 QuoteWordsAtom@19-32 ColonPair::Value@19-32 QuotedString@25-32 StrLiteral@26-31 StrLiteral@32-33 QuoteWordsAtom@33-41 ColonPair::Value@33-41 QuotedString@38-41 StrLiteral@39-40 StrLiteral@41-42 Type::Simple@4-7 Name@4-7',
    'a typed enum over colonpair words';
