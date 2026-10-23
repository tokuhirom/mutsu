use Test;

# A statement's range ends at its last token, not after a comment that
# follows it, and a `StatementList` / `SemiList` runs from its first
# statement to the `;` after its last, whitespace and comments before each
# `;` included (ADR-10723 Stage 2). Ranges measured on rakudo 2026.09.

plan 14;

# The ranges of the statement lists and statements of `$src`, in walk order.
sub lists(Str $src) {
    my @r;
    sub walk($node) {
        my $name = $node.^name.subst('RakuAST::', '');
        if $name eq 'StatementList' | 'SemiList' | 'Statement::Expression' {
            @r.push: "$name {.from}..{.to}" with $node.origin;
        }
        $node.visit-children(&walk);
    }
    $src.AST.visit-children(&walk);
    @r.join(', ')
}

is lists("my \$x = \{\n  # comment\n  1;\n\};"),
    'Statement::Expression 0..28, StatementList 24..26, Statement::Expression 24..25',
    'a comment before the first statement of a block';
is lists("my \$x = \{ # c\n  1 # d\n\};"),
    'Statement::Expression 0..23, StatementList 16..17, Statement::Expression 16..17',
    'a comment after the last statement is not part of it';
is lists('my $x = { 1 ; };'),
    'Statement::Expression 0..15, StatementList 10..13, Statement::Expression 10..11',
    'whitespace before a terminator is part of the list';
is lists("if 1 \{\n  2 ;\n\}"),
    'StatementList 9..12, Statement::Expression 9..10',
    'the block of a statement-level construct';
is lists("for 1 \{ # c\n  1 ; # d\n\}"),
    'StatementList 14..17, Statement::Expression 14..15',
    'a loop block with comments around a spaced terminator';
is lists("sub f \{ # c\n  1 ; # d\n\}"),
    'Statement::Expression 0..23, StatementList 14..17, Statement::Expression 14..15',
    'a routine body';
is lists("my \$x = -> \$a \{ # c\n  1 # d\n\};"),
    'Statement::Expression 0..29, StatementList 22..23, Statement::Expression 22..23',
    'a pointy block';
is lists("class A \{ # c\n\}"),
    'Statement::Expression 0..15, StatementList 9..9',
    'an empty list sits just after the brace';
is lists("my \$x = (1, # c\n 2 # d\n);"),
    'Statement::Expression 0..24, SemiList 9..18, Statement::Expression 9..18',
    'a parenthesized list with comments between and after its operands';
is lists("my \@a; \@a[ 0 # c\n];"),
    'Statement::Expression 0..5, Statement::Expression 7..18, SemiList 11..12, Statement::Expression 11..12',
    'a subscript with a comment after its index';
is lists('my $x = (  );'),
    'Statement::Expression 0..12, SemiList 9..9',
    'an empty parenthesized list sits just after the paren';
is lists("# c\nsay 1 ;  # x\n"),
    'Statement::Expression 4..9',
    'a top-level statement followed by a spaced terminator and a comment';
is "# c\nsay 1 ;  # x\n".AST.origin.from ~ '..' ~ "# c\nsay 1 ;  # x\n".AST.origin.to,
    '4..11', 'the unit list runs to its last terminator';
is lists("say 1; say 2 # y\n;\n"),
    'Statement::Expression 0..5, Statement::Expression 7..12',
    'a comment between a statement and its terminator';
