use Test;

plan 4;

# `$obj.accessor[i][j] = v` where `accessor` returns a plain Array-of-Arrays
# attribute (`has @.cell`) must write through both index levels back into the
# attribute, the same as a direct `@!attr[i][j] = v` would.

class Table {
    has @.cell;
}

my $t = Table.new(cell => [[1, 2, 3], [4, 5, 6]]);
$t.cell[0][1] = 48;
is $t.cell[0][1], 48, 'array-of-arrays accessor: nested index write then read back';
is $t.raku, 'Table.new(cell => [[1, 48, 3], [4, 5, 6]])', 'array-of-arrays accessor: .raku reflects the write';

class Rows {
    has %.row;
}

my $r = Rows.new(row => { a => [1, 2], b => [3, 4] });
$r.row<a>[1] = 99;
is $r.row<a>[1], 99, 'hash-of-arrays accessor: nested key+index write then read back';
is $r.row<b>[0], 3, 'hash-of-arrays accessor: sibling entry unaffected';
