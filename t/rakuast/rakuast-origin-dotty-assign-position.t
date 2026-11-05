use Test;
use experimental :rakuast;

# A `.=` application that is not a statement of its own carries the same
# origins as one that is (#15780): as a call argument, and inside parentheses,
# where the `SemiList` and `Statement::Expression` around it get the range of
# the text inside the brackets. Ranges measured on rakudo 2026.09.

plan 4;

# Each visited node of `$src`, with its range.
sub nodes(Str $src) {
    my @r;
    sub walk($node) {
        @r.push: $node.^name.subst('RakuAST::', '') ~ ' '
            ~ ($node.origin andthen "{.from}..{.to}" orelse '-');
        $node.visit-children(&walk);
    }
    $src.AST.visit-children(&walk);
    @r
}

is nodes('my $a; say ($a .= lc);').join(', '),
    'Statement::Expression 0..5, VarDeclaration::Simple 0..5, Statement::Expression 7..21, Call::Name::WithoutParentheses 7..21, Name 7..10, ArgList 11..21, Circumfix::Parentheses 11..21, SemiList 12..20, Statement::Expression 12..20, ApplyDottyInfix 12..20, Var::Lexical 12..14, DottyInfix::CallAssign 15..17, Call::Method 18..20, Name 18..20, ArgList 20..20',
    'in parentheses';

is nodes('my @a; say(@a .= reverse);').join(', '),
    'Statement::Expression 0..5, VarDeclaration::Simple 0..5, Statement::Expression 7..25, Call::Name 7..25, Name 7..10, ArgList 11..24, ApplyDottyInfix 11..24, Var::Lexical 11..13, DottyInfix::CallAssign 14..16, Call::Method 17..24, Name 17..24, ArgList 24..24',
    'as the argument of a call with parentheses';

is nodes('my @a; say @a .= sort;').join(', '),
    'Statement::Expression 0..5, VarDeclaration::Simple 0..5, Statement::Expression 7..21, Call::Name::WithoutParentheses 7..21, Name 7..10, ArgList 11..21, ApplyDottyInfix 11..21, Var::Lexical 11..13, DottyInfix::CallAssign 14..16, Call::Method 17..21, Name 17..21, ArgList 21..21',
    'as the argument of a call without parentheses';

is nodes('my $a; say(( $a .= lc ));').join(', '),
    'Statement::Expression 0..5, VarDeclaration::Simple 0..5, Statement::Expression 7..24, Call::Name 7..24, Name 7..10, ArgList 11..23, Circumfix::Parentheses 11..23, SemiList 13..21, Statement::Expression 13..21, ApplyDottyInfix 13..21, Var::Lexical 13..15, DottyInfix::CallAssign 16..18, Call::Method 19..21, Name 19..21, ArgList 21..21',
    'in parentheses with blanks, as an argument';

