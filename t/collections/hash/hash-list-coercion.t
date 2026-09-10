use Test;
plan 8;

# `.List` on a Hash is its pairs, exactly like `.list` and `.Array`. It used to
# fall through to the scalar catch-all and produce a one-element list holding
# the whole Hash — which is how `Cro::HTTP::Client`'s
# `self!set-headers($request, $value.List)` saw a Hash where a Pair was
# required and rejected every `headers => %h`.

my %h = a => 1, b => 2;
is %h.List.elems, 2, 'Hash.List has one element per pair';
is %h.List.sort(*.key).map(*.key).join(','), 'a,b', 'Hash.List yields the keys';
ok %h.List.all ~~ Pair, 'every Hash.List element is a Pair';
is %h.List.sort(*.key).raku, %h.list.sort(*.key).raku, 'Hash.List agrees with Hash.list';

is set(1, 2).List.elems, 2, 'Set.List has one element per member';
ok set(1, 2).List.all ~~ Pair, 'every Set.List element is a Pair';
is bag(1, 1, 2).List.sort(*.key).map(*.value).join(','), '2,1', 'Bag.List carries the weights';

is (a => 1).List.raku, '(:a(1),)', 'Pair.List is still a one-element list';
