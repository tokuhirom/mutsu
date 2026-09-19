use Test;

# Regression reduced from Data::StaticTable 0.1.1's typed Position helpers.
plan 4;

module CompoundSubsetScope {
    subset Table::Position of Int where * >= 1;

    class Table {
        method !dimensions(Position $columns) {
            my Position $checked = $columns;
            $checked
        }
        method make(@values) { self!dimensions(@values.elems) }
    }
}

is CompoundSubsetScope::Table::Position.^name,
    'CompoundSubsetScope::Table::Position',
    'a compound subset in a package has the package-qualified identity';
ok 1 ~~ CompoundSubsetScope::Table::Position,
    'the package-qualified subset accepts a conforming value';
is CompoundSubsetScope::Table.make(@(1)), 1,
    'a typed method parameter resolves the compound subset in its package';
is CompoundSubsetScope::Table.make(@(1, 2, 3)), 3,
    'a private method call accepts the same compound subset type';
