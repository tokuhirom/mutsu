use Test;

# A `;` that terminates no statement is a `Statement::Empty` with a zero-width
# origin at that `;` (#14820). Measured on rakudo 2026.09.

plan 15;

sub empties(Str $source) { $source.AST.statements.grep(RakuAST::Statement::Empty).elems }
sub empty-origins(Str $source) {
    $source.AST.statements.grep(RakuAST::Statement::Empty).map({ .origin.from ~ '..' ~ .origin.to }).join(',')
}

# --- read direction ------------------------------------------------------

is empties('say 1;; say 2'), 1, 'a doubled `;` leaves one empty statement';
is empties('say 1;; say 2;;; say 3'), 3, 'each extra `;` is one';
is empties(';; say 1'), 2, 'leading separators are empty statements';
is empties('say 1;;'), 1, 'a trailing doubled `;` is one';
is empties('say 1;'), 0, 'a statement terminator is not an empty statement';
is empties('if 1 { say 1 }; say 2'), 0, 'the `;` after a block statement is its terminator';
is empties('say 1; ; say 2'), 1, 'separators with whitespace between them';

is empty-origins('say 1;;; say 2'), '6..6,7..7', 'each empty statement is zero-width at its `;`';
is empty-origins('say 1; ; say 2'), '7..7', 'the origin skips the whitespace';
is empty-origins(';; say 1'), '0..0,1..1', 'leading empty statements are placed';

is Q[my $x = { 1;; 2 }].AST.statements[0].expression.initializer.expression.body.statement-list.statements.grep(RakuAST::Statement::Empty).elems,
    1, 'inside a block body';

# --- write direction -----------------------------------------------------

is EVAL(Q[5;;].AST), 5, 'a trailing empty statement is not the value';
is EVAL(Q[my $x = do { 5;; }; $x].AST), 5, 'nor inside a do block';
is EVAL(Q[my $c = { 1;; 2;; }; $c()].AST), 2, 'nor inside a bare block';
is EVAL(Q[;; 7].AST), 7, 'leading empty statements run nothing';
