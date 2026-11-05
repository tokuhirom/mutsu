use Test;

# A Pod `table` block is a `RakuAST::Doc::Block` of `RakuAST::Doc::LegacyRow`
# rows in its `.AST` (#14880), shaped as rakudo 2026.09 shapes them. Whitespace
# tables split a row where two or more spaces separate the cells, pipe tables
# at each `|`; a blank line or a rule (`--`, `==`) between rows is kept as a
# string and, when it makes a header, adds `header-row => 0` to the block.
# (Tables whose columns rakudo infers by rules not read here, such as rows
# that do not line up, are left out of mutsu's tree; this file pins only the
# shapes both read the same.)

plan 19;

use experimental :rakuast;

sub rows(Str $src) {
    my $block = $src.AST.statements[0];
    $block.paragraphs.map({
        when RakuAST::Doc::LegacyRow {
            my $d = (try .column-dividers) // '';
            "row({.column-offsets.join(',')}|{.cells.map({"[$_]"}).join}{$d ?? "|div=$d" !! ''})"
        }
        default { "str({.raku})" }
    }).join(' ')
}

sub block-of(Str $body) { "=begin table\n$body\n=end table\n" }

is rows(block-of("A  B\nC  D")), 'row(0,3|[A][B]) row(0,3|[C][D])',
    'two columns split at a gap of two spaces';
is rows(block-of("A   B   C\nD   E   F")), 'row(0,4,8|[A ][B ][C]) row(0,4,8|[D ][E ][F])',
    'a wider gap stays in the cell, up to two characters before the next column';
is rows(block-of("A B\nC D")), 'row(0|[A B]) row(0|[C D])', 'single spaces are one cell';
is rows(block-of("AAA  B\nC    D")), 'row(0,5|[AAA][B]) row(0,5|[C  ][D])', 'cells of different widths';
is rows(block-of("A | B | C\nD | E | F")), 'row(4,8|[A][B][C]|div=||) row(4,8|[D][E][F]|div=||)',
    'pipe table: an offset after each divider';
is rows(block-of("| A | B |\n| C | D |")), 'row(2,6,10|[A][B]|div=|||) row(2,6,10|[C][D]|div=|||)',
    'pipe table with outer dividers';
is rows(block-of("A |  B\nCC  | D")), 'row(4|[A][ B]|div=|) row(6|[CC ][D]|div=|)',
    'pipe table: one blank is trimmed on each side, each row has its own offsets';

is rows(block-of("A  B\n--\nC  D")), 'row(0,3|[A][B]) str("--") row(0,3|[C][D])',
    'a rule between rows is a string';
is rows(block-of("A  B\nC  D\n--\nE  F")), 'row(0,3|[A' ~ "\n" ~ 'C][B' ~ "\n" ~ 'D]) str("--") row(0,3|[E][F])',
    'the rows before the rule are one multi-line header row';
is rows(block-of("A  B\n---\nC  D\n---\nE  F")), 'row(0,3|[A][B]) str("---") row(0,3|[C][D]) str("---") row(0,3|[E][F])',
    'equal rules are not a header';
is rows(block-of("| A | B\n--+--\n| C | D")), 'row(2,6|[A][B]|div=||) str("--+--\n") row(2,6|[C][D]|div=||)',
    'the rule of a pipe table keeps its line break';

is block-of("A  B\n--\nC  D").AST.statements[0].config.raku,
    'Map.new((:header-row(RakuAST::IntLiteral.new(0))))', 'a header rule sets header-row';
nok block-of("A  B\n---\nC  D\n---\nE  F").AST.statements[0].config, 'equal rules set no config';
nok block-of("A  B\nC  D").AST.statements[0].config, 'a plain table sets no config';

is "=table\nA  B\nC  D\n\nsay 1;\n".AST.statements[0].raku.subst(/\s+/, ' ', :g),
    'RakuAST::Doc::Block.new( type => "table", abbreviated => True, paragraphs => ( RakuAST::Doc::LegacyRow.new( column-offsets => (0,3), cells => ( "A", "B", ) ), RakuAST::Doc::LegacyRow.new( column-offsets => (0,3), cells => ( "C", "D", ) ), ) )',
    'an abbreviated table runs to the blank line';
is rows("=for table\nA  B\nC  D\n\nsay 1;\n"), 'row(0,3|[A][B]) row(0,3|[C][D])', '=for table';

# A hand-built row prints as rakudo prints it.
my $row = RakuAST::Doc::LegacyRow.new(column-dividers => "|", column-offsets => (4,), cells => ("A", "B"));
is $row.raku.subst(/\s+/, ' ', :g),
    'RakuAST::Doc::LegacyRow.new( column-dividers => "|", column-offsets => (4,), cells => ( "A", "B", ) )',
    'a hand-built pipe row';
is $row.cells.join(','), 'A,B', 'cells accessor';
is RakuAST::Doc::LegacyRow.new(column-offsets => (0,3), cells => ("A", "B")).column-offsets.join(','),
    '0,3', 'column-offsets accessor';
