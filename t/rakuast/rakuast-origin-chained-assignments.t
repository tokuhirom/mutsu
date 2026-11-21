use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: in a chained assignment
# (`$x = $y = 1`, `@a[3] = @a[2] = 8`), the inner assignment the parser left
# unplaced runs from after the outer `=` to the end, with its parts.

plan 4;
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
is placed(Q[my $x; my $y; $x = $y = 1]),
    'VarDeclaration::Simple@0-5 VarDeclaration::Simple@7-12 ApplyInfix@14-25 Assignment@17-18 ArgList@14-25 Var::Lexical@14-16 ApplyInfix@19-25 Assignment@22-23 ArgList@19-25 Var::Lexical@19-21 IntLiteral@24-25',
    'a chained scalar assignment';
is placed(Q[my @a; @a[3] = @a[2] = 8]),
    'VarDeclaration::Simple@0-5 ApplyPostfix@7-24 Var::Lexical@7-9 Postcircumfix::ArrayIndex@9-24 SemiList@10-11 IntLiteral@10-11 ApplyPostfix@15-24 Var::Lexical@15-17 Postcircumfix::ArrayIndex@17-24 SemiList@18-19 IntLiteral@18-19 IntLiteral@23-24',
    'a chained subscript assignment';
is placed(Q[my %h; %h{"a"} = %h{"b"} = 1]),
    'VarDeclaration::Simple@0-5 ApplyInfix@7-28 Assignment@15-16 ArgList@7-28 ApplyPostfix@7-14 Var::Lexical@7-9 Postcircumfix::HashIndex@9-14 SemiList@10-13 QuotedString@10-13 StrLiteral@11-12 ApplyInfix@17-28 Assignment@25-26 ArgList@17-28 ApplyPostfix@17-24 Var::Lexical@17-19 Postcircumfix::HashIndex@19-24 SemiList@20-23 QuotedString@20-23 StrLiteral@21-22 IntLiteral@27-28',
    'a chained hash subscript assignment';
is placed(Q[my $x; my $y; $x = $y + 1]),
    'VarDeclaration::Simple@0-5 VarDeclaration::Simple@7-12 ApplyInfix@14-25 Assignment@17-18 ArgList@14-25 Var::Lexical@14-16 ApplyInfix@19-25 Infix@22-23 ArgList@19-25 Var::Lexical@19-21 IntLiteral@24-25',
    'an assignment of an infix expression';
