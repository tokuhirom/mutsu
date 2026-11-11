use Test;

# `flat` descends into a `Channel.list` held in a List. That list is pulled on
# demand and has nothing cached until then, and the pure flattener read the
# empty cache: `(Channel.list, 9).flat` was `(9)` (#17435).

plan 4;

sub chan(*@v) { my $c = Channel.new; $c.send($_) for @v; $c.close; $c.list }

is-deeply (chan(3, 1, 2), 9).flat.List, (3, 1, 2, 9), '(Channel.list, 9).flat';
is-deeply (chan(5),).flat.List, (5,), 'a one-element List holding it';
is-deeply flat(chan(3, 1, 2), 7).List, (3, 1, 2, 7), 'the flat sub';
is-deeply (1, (chan(3, 1), 4)).flat.List, (1, 3, 1, 4), 'nested one level down';
