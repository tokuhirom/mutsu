use Test;

plan 3;

# A hyper operand that is an Array element's container (`.head`, `.tail`)
# hypers element-wise, not as its numified `.elems`.
my @a = (1, 2, 3), (4, 5, 6);
is-deeply ((1, 2, 3) <<*>> @a.head).List, (1, 4, 9), '<<*>> @a.head';
is-deeply (@a.tail >>+>> 1).List, (5, 6, 7), '@a.tail >>+>> 1';
is-deeply ((1, 2, 3) <<*>> $(1, 2, 3)).List, (1, 4, 9), 'an itemized list still hypers';
