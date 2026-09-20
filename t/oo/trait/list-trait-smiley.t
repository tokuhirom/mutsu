use Test;

# Pod::Contents uses the `is List:D` spelling for its typed list variables.
plan 5;

my @items is List:D = 1, 2, 3;

is @items.^name, 'List', '`is List:D` applies the List trait';
is @items.raku, '(1, 2, 3)', '`is List:D` preserves the initializer';
isa-ok @items, List, '`is List:D` produces a List';
throws-like { @items = 4, 5, 6 }, X::Assignment::RO,
    '`is List:D` keeps the List container immutable';

my @other-items is List:U = 7, 8;
is @other-items.^name, 'List', '`is List:U` accepts the same variable-trait spelling';
