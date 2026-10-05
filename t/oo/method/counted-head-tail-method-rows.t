use Test;

# ADR-11276 slice 3: the counted Any.head and Any.tail forms use one handler
# row across plain receiver shapes. Array element cells stay writable, and a
# non-plain argument still takes the existing cascade path.

plan 12;

my $array = [1, 2, 3, 4];
my $list = (5, 6, 7, 8).List;

is-deeply $array.head(2).List, (1, 2).List, 'Array.head(count)';
is-deeply $array.tail(2).List, (3, 4).List, 'Array.tail(count)';
is-deeply $list.head(2).List, (5, 6).List, 'List.head(count)';
is-deeply $list.tail(2).List, (7, 8).List, 'List.tail(count)';
is-deeply $array.tail(-1).List, ().List, 'Array.tail rejects a negative count';
is-deeply $list.tail(-1).List, ().List, 'List.tail rejects a negative count';
is-deeply 42.tail(-1).List, ().List, 'Any.tail rejects a negative count';
is Any.^lookup('head').package.^name, 'Any', 'Rakudo declares counted head on Any';

sub prefix($items) { $items.head(2).List }
is-deeply prefix($array), (1, 2).List, 'cached call site answers an Array';
is-deeply prefix($list), (5, 6).List, 'same call site answers a List';

my $mutable = [1, 2, 3];
for $mutable.head(2) { $_ += 10 }
is-deeply $mutable.List, (11, 12, 3).List, 'head keeps mutable Array element cells';

is-deeply (1, 2, 3).List.head(* - 1).List, (1, 2).List,
    'WhateverCode argument falls through to the cascade';
