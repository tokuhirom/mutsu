use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a variable's type trait
# (`my %h is MixHash`) places its type; a dotted subscript (`$x.[0]`,
# `*.<k>`) starts after its dot and its operand variable is placed; and the
# rebuilt colonpair of `is built(:bind)` is placed inside its parentheses.

plan 4;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) unless $class ~~ /^Statement/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[my %h is MixHash;]),
    'VarDeclaration::Simple@0-16 Trait::Is@6-16 Type::Simple@9-16 Name@9-16',
    'a type trait on a variable';
is placed(Q[my $x; $x.[0];]),
    'VarDeclaration::Simple@0-5 ApplyPostfix@7-13 Var::Lexical@7-9 Postcircumfix::ArrayIndex@10-13 SemiList@11-12 IntLiteral@11-12',
    'a dotted array subscript';
is placed(Q[my @a; @a.map(*.<k>);]),
    'VarDeclaration::Simple@0-5 ApplyPostfix@7-20 Var::Lexical@7-9 Call::Method@10-20 Name@10-13 ArgList@14-19 ApplyPostfix@14-19 Postcircumfix::LiteralHashIndex@16-19 QuotedString@17-18 StrLiteral@17-18',
    'a dotted literal hash subscript on a whatever';
is placed(Q[class C { has $.f is built(:bind) }]),
    'Class@0-35 Name@6-7 Block@8-35 Blockoid@8-35 VarDeclaration::Simple@10-33 Trait::Is@18-33 Name@21-26 Circumfix::Parentheses@26-33 SemiList@27-32 ColonPair::True@27-32',
    'an attribute is built(:bind)';
