use Test;

plan 3;

is "ab".NFD.list.join(','), '97,98', 'NFD.list before the wrap';

Uni.^find_method('list').wrap(-> | { "uni-list" });

is "ab".NFD.list, 'uni-list', 'a wrap of Uni.list is honoured for an NFD value';
is "ab".NFC.list, 'uni-list', 'and for an NFC value';
