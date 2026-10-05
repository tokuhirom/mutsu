use Test;

plan 4;

# Rakudo declares these as two rows, and the table routes both plain
# positional collection shapes through their shared reverse handler.
is List.^lookup('reverse').package.^name, 'List',
    'List.reverse is declared on List';
is Array.^lookup('reverse').package.^name, 'Array',
    'Array.reverse is declared on Array';

my $list = (1, 2, 3);
is $list.reverse.raku, '(3, 2, 1).Seq',
    'List.reverse returns a reversed Seq';

my @array = 1, 2, 3;
is @array.reverse.raku, '(3, 2, 1).Seq',
    'Array.reverse returns a reversed Seq';
