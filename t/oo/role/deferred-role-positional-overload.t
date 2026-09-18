use v6;
use Test;

plan 5;

role Row {
    has Any @.data;

    method values { @!data.join(',') }
}

role Table does Positional {
    has Any @.data = [['a', 1], ['b', 2]];

    method elems { @!data.elems }

    method extend {
        @!data[0;2] = 3;
        @!data[1;2] = 4;
    }
}

multi postcircumfix:<[ ]>(Table:D $table, Int $row) {
    Row.new(data => $table.data[$row;*])
}

multi postcircumfix:<[ ]>(Table:D $table, Whatever) {
    (0 ..^ $table.elems).map({ $table[$_] })
}

my $table = Table.new;
$table.extend;

is $table.data[0;*].join(','), 'a,1,3',
    'the role-punned table keeps its extended row';
is $table[*].map(*.values).join('|'), 'a,1,3|b,2,4',
    'a lazy whatever slice preserves role attribute overrides';

class DeferredRows {
    method rows {
        my @values = <a b>;
        <x y>.map({ @values[$++] })
    }
}

my $source = DeferredRows.new;
is $source.rows.Array.join(','), 'a,b',
    'a deferred map keeps anonymous state scoped to its creating call';
is $source.rows.Array.join(','), 'a,b',
    'the next deferred map call gets a fresh anonymous state';

class Cell {
    has Int $.value;
}

multi postcircumfix:<[ ]>(Cell @cells, Whatever) {
    @cells.map(*.value).join(',')
}

my Cell @cells = Cell.new(value => 1), Cell.new(value => 2);
is @cells[*], '1,2',
    'a typed array can dispatch its Whatever postcircumfix overload';
