use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: the subscript a binding
# assigns to (`@a[0] := $v`), which the parser left unplaced, runs from the
# start of the binding to the blanks before `:=`, with its parts.

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
is placed(Q[my @a; my $v; @a[0] := $v]),
    'VarDeclaration::Simple@0-5 VarDeclaration::Simple@7-12 ApplyInfix@14-25 Infix@20-22 ArgList@14-25 ApplyPostfix@14-19 Var::Lexical@14-16 Postcircumfix::ArrayIndex@16-19 SemiList@17-18 IntLiteral@17-18 Var::Lexical@23-25',
    'a subscript bound to';
is placed(Q[my %h; %h<q> := 1]),
    'VarDeclaration::Simple@0-5 ApplyInfix@7-17 Infix@13-15 ArgList@7-17 ApplyPostfix@7-12 Var::Lexical@7-9 Postcircumfix::LiteralHashIndex@9-12 QuotedString@10-11 StrLiteral@10-11 IntLiteral@16-17',
    'a word subscript bound to';
is placed(Q[my @a; @a[0] := 1 => 2]),
    'VarDeclaration::Simple@0-5 ApplyInfix@7-22 Infix@13-15 ArgList@7-22 ApplyPostfix@7-12 Var::Lexical@7-9 Postcircumfix::ArrayIndex@9-12 SemiList@10-11 IntLiteral@10-11 ApplyInfix@16-22 Infix@18-20 ArgList@16-22 IntLiteral@16-17 IntLiteral@21-22',
    'a subscript bound to a pair';
