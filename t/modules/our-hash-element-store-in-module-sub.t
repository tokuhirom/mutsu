use Test;

# A module routine storing into its package's own `our %h` element by element
# must update the one container every read sees. The routine's captured env
# cell used to take the store, so the key vanished (ake's `%TASKS{$name} = ...`).

plan 5;

module M {
    our %T;
    our @A;
    our sub add($n) { %T{$n} = 1; @A[@A.elems] = $n; %T.keys.sort.List }
    our sub bump($n) { %T{$n}++; %T{$n} }
}

is-deeply M::add('a'), ('a',), 'the store is visible to the same routine';
is-deeply M::add('b'), ('a', 'b'), 'a second store keeps the first key';
is-deeply %M::T.keys.sort.List, ('a', 'b'), 'the package variable holds both keys';
is-deeply @M::A.List, ('a', 'b'), 'the our array sees its element stores';
is M::bump('a'), 2, 'an element increment updates the package hash';
