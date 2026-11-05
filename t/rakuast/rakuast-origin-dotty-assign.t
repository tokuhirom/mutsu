use Test;
use experimental :rakuast;

# `VAR .= name` carries the origins rakudo gives its parts (#15780): the
# variable, the `.=` operator, the `Method` call from its name to its end, its
# `Name` and its `ArgList` (empty right after the name, just inside the
# parentheses, or from the first argument to the last). Ranges measured on
# rakudo 2026.09.

plan 5;

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

is nodes('my $f; $f .= uc;').join(', '),
    'Statement::Expression 0..5, VarDeclaration::Simple 0..5, Statement::Expression 7..15, ApplyDottyInfix 7..15, Var::Lexical 7..9, DottyInfix::CallAssign 10..12, Call::Method 13..15, Name 13..15, ArgList 15..15',
    'a scalar with a bare method';

is nodes('my @a; @a .= sort;').join(', '),
    'Statement::Expression 0..5, VarDeclaration::Simple 0..5, Statement::Expression 7..17, ApplyDottyInfix 7..17, Var::Lexical 7..9, DottyInfix::CallAssign 10..12, Call::Method 13..17, Name 13..17, ArgList 17..17',
    'an array with a bare method';

is nodes('my $f; $f .= substr(1, 2);').join(', '),
    'Statement::Expression 0..5, VarDeclaration::Simple 0..5, Statement::Expression 7..25, ApplyDottyInfix 7..25, Var::Lexical 7..9, DottyInfix::CallAssign 10..12, Call::Method 13..25, Name 13..19, ArgList 20..24, IntLiteral 20..21, IntLiteral 23..24',
    'arguments in parentheses';

is nodes('my $f; $f .= uc();').join(', '),
    'Statement::Expression 0..5, VarDeclaration::Simple 0..5, Statement::Expression 7..17, ApplyDottyInfix 7..17, Var::Lexical 7..9, DottyInfix::CallAssign 10..12, Call::Method 13..17, Name 13..15, ArgList 16..16',
    'empty parentheses';

is nodes('my @a; @a .= map: { $_ + 1 };').join(', '),
    'Statement::Expression 0..5, VarDeclaration::Simple 0..5, Statement::Expression 7..28, ApplyDottyInfix 7..28, Var::Lexical 7..9, DottyInfix::CallAssign 10..12, Call::Method 13..28, Name 13..16, ArgList 18..28, Block 18..28, Blockoid 18..28, StatementList 20..26, Statement::Expression 20..26, ApplyInfix 20..26, Infix 23..24, ArgList 20..26, Var::Lexical 20..22, IntLiteral 25..26',
    'a colon argument list';

