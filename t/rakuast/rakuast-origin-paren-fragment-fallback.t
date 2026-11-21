use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a lone expression in
# parentheses whose terms share their shape with other terms of the
# statement (`((1, 2) Z (3, 4)).List, 1`) is placed from a fragment parse of
# the text inside the parentheses.

plan 2;
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
is placed(Q[say ((1, 2) Z (3, 4) Z (5, 6)).List, 1]),
    'Call::Name::WithoutParentheses@0-38 Name@0-3 ArgList@4-38 ApplyPostfix@4-35 ApplyListInfix@5-29 Infix@21-22 Circumfix::Parentheses@5-11 SemiList@6-10 ApplyListInfix@6-10 Infix@7-8 IntLiteral@6-7 IntLiteral@9-10 Circumfix::Parentheses@14-20 SemiList@15-19 ApplyListInfix@15-19 Infix@16-17 IntLiteral@15-16 IntLiteral@18-19 Circumfix::Parentheses@23-29 SemiList@24-28 ApplyListInfix@24-28 Infix@25-26 IntLiteral@24-25 IntLiteral@27-28 Call::Method@31-35 Name@31-35 ArgList@35-35 IntLiteral@37-38',
    'a zip chain beside a term of the same shape';
is placed(Q[say (1, 2), 1]),
    'Call::Name::WithoutParentheses@0-13 Name@0-3 ArgList@4-13 Circumfix::Parentheses@4-10 SemiList@5-9 ApplyListInfix@5-9 Infix@6-7 IntLiteral@5-6 IntLiteral@8-9 IntLiteral@12-13',
    'a parenthesized list beside a term of the same shape';
