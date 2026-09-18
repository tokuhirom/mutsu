use Test;
plan 1;

class Matrix {
    has @.rows;

    method AT-POS(Int:D $row, Int:D $column) {
        @!rows[$row;$column]
    }
}

my $matrix = Matrix.new(rows => [[1, 2], [3, 4]]) but Positional;
is $matrix[1;0], 3,
    'multidimensional indexing of a role-punned object keeps value semantics in call arguments';
