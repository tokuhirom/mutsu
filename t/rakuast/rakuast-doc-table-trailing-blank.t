use experimental :rakuast;
use Test;

# A Pod `table` row that ends in a blank is read as rakudo's RakuAST frontend
# reads it (2026.11; #18290): the RakuAST view used to leave the whole block
# out of the unit's statements. After a final divider the blanks are one more
# cell (less the divider's own blank); after cell text they stay in the cell.

plan 8;

sub rows(Str $table) {
    my $block = ("=table\n" ~ $table ~ "\nsay 1;\n").AST.statements[0];
    return Nil unless $block ~~ RakuAST::Doc::Block;
    $block.paragraphs.map({
        $_ ~~ RakuAST::Doc::LegacyRow ?? [.column-offsets.List, .cells.List] !! $_
    }).List
}

is-deeply ("=table\nX | O | \n---+---+---\n  | X | O\n\nsay 1;\n").AST.statements.map(*.^name).List,
    ("RakuAST::Doc::Block", "RakuAST::Statement::Expression"),
    'a table whose row ends in "| " is a statement of the unit';

is-deeply rows("X | O | \n---+---+---\n  | X | O\n"),
    ([(4, 8), ("X", "O", "")], "---+---+---\n", [(4, 8), (" ", "X", "O")]),
    'the blank after the last divider is an empty cell';

is-deeply rows("X | O |  \nA | B | C\n"),
    ([(4, 8), ("X", "O", " ")], [(4, 8), ("A", "B", "C")]),
    'a second blank after the last divider stays in that cell';

is-deeply rows("a | b \nc | d\n"),
    ([(4,), ("a", "b ")], [(4,), ("c", "d")]),
    'a blank after the last cell stays in the cell';

is-deeply rows("a | b\nc | d \n"),
    ([(4,), ("a", "b")], [(4,), ("c", "d ")]),
    'on any row of the table';

is-deeply rows("| a | b | \n| c | d | \n"),
    ([(2, 6, 10), ("a", "b", "")], [(2, 6, 10), ("c", "d", "")]),
    'a row framed by dividers ends in an empty cell';

is-deeply rows("a  b \nc  d\n"),
    ([(0, 3), ("a", "b ")], [(0, 3), ("c", "d")]),
    'a whitespace table keeps the blank in its last cell';

is-deeply rows("X | O |\n---+---+---\n  | X | O\n"),
    ([(4, 8), ("X", "O")], "---+---+---\n", [(4, 8), (" ", "X", "O")]),
    'a row ending in its divider has no extra cell';
