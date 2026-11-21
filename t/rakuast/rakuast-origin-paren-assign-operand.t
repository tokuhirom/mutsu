use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: an element assignment in
# parentheses that a method is called on (`(@a[0] = 7).say`), whose
# parentheses Rakudo drops, is placed from a fragment parse of the text
# inside them, statements nested in its subscript included.

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
is placed(Q[my @a; (@a[0] = 7).say]),
    'VarDeclaration::Simple@0-5 ApplyPostfix@7-22 ApplyPostfix@8-17 Var::Lexical@8-10 Postcircumfix::ArrayIndex@10-17 SemiList@11-12 IntLiteral@11-12 IntLiteral@16-17 Call::Method@19-22 Name@19-22 ArgList@22-22',
    'an array element assignment in parentheses, called on';
is placed(Q[my %h; (%h<b> = 7).raku]),
    'VarDeclaration::Simple@0-5 ApplyPostfix@7-23 ApplyPostfix@8-17 Var::Lexical@8-10 Postcircumfix::LiteralHashIndex@10-17 QuotedString@11-12 StrLiteral@11-12 IntLiteral@16-17 Call::Method@19-23 Name@19-23 ArgList@23-23',
    'a hash element assignment in parentheses, called on';
is placed(Q[my @a; (@a[1] = 7 + 1).say]),
    'VarDeclaration::Simple@0-5 ApplyPostfix@7-26 ApplyPostfix@8-21 Var::Lexical@8-10 Postcircumfix::ArrayIndex@10-21 SemiList@11-12 IntLiteral@11-12 ApplyInfix@16-21 Infix@18-19 ArgList@16-21 IntLiteral@16-17 IntLiteral@20-21 Call::Method@23-26 Name@23-26 ArgList@26-26',
    'an element assigned an infix application';
is placed(Q[my %h; say (%h{"b"} = 2).WHAT]),
    'VarDeclaration::Simple@0-5 Call::Name::WithoutParentheses@7-29 Name@7-10 ArgList@11-29 ApplyPostfix@11-29 ApplyInfix@12-23 Assignment@20-21 ArgList@12-23 ApplyPostfix@12-19 Var::Lexical@12-14 Postcircumfix::HashIndex@14-19 SemiList@15-18 QuotedString@15-18 StrLiteral@16-17 IntLiteral@22-23 Call::Method@25-29 Name@25-29 ArgList@29-29',
    'a quoted hash key assignment as a call argument';
