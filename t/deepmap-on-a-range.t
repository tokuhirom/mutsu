use v6;
use Test;

plan 11;

# A Range is Iterable, so the *map family descends into it exactly as it
# descends into a List. mutsu used to treat a Range as a *leaf*: the block was
# handed the whole Range once and `(1..4).deepmap({ $_ * 2 })` answered the
# Range `2..8` without ever calling the block per element.

is (1..4).deepmap({ $_ * 2 }).raku, '(2, 4, 6, 8)', 'deepmap descends into a Range';
is (1..4).nodemap({ $_ * 2 }).raku, '(2, 4, 6, 8)', 'nodemap descends into a Range';
is (1..4).duckmap({ $_ * 2 }).raku, '(2, 4, 6, 8)', 'duckmap descends into a Range';

is (1..4).deepmap({ $_ * 2 }).WHAT.gist, '(List)', 'deepmap over a Range answers a List';

is (1^..^5).deepmap(* + 1).raku, '(3, 4, 5)', 'an exclusive Range descends too';

# The consequence past the wrong value: with the Range treated as a leaf there
# was no loop for a `next` to belong to, so it escaped as X::ControlFlow.
is (1..4).deepmap({ next if $_ %% 2; $_ }).raku, '(1, 3)', 'next inside a deepmap over a Range';

# A nested Range itemizes like a nested List does.
is (1, (2..3)).deepmap(* + 1).raku, '(2, $(3, 4))', 'a Range nested in a List itemizes';
is %(a => 1, b => (2..3)).deepmap(* + 1).raku, '{:a(2), :b($(3, 4))}',
    'a Range nested in a Hash itemizes';

# nodemap does not descend, so a nested Range reaches the block whole.
is (1, (2..3)).nodemap(*.elems).raku, '(1, 2)', 'nodemap does not descend into a nested Range';

# duckmap descends only when the block rejects the value. Compared against the
# equivalent List rather than a literal: mutsu does not itemize a duckmap
# descend at all (`(10, (20, 30))` where raku says `(10, $(20, 30))`), which is
# pre-existing and the same for a List — see
# todo/tickets/duckmap-does-not-itemize-a-nested-descend.md. What this asserts
# is the invariant the fix establishes: a Range descends like its List.
is (1, (2..3)).duckmap(-> Int $x { $x * 10 }).raku,
   (1, (2, 3)).duckmap(-> Int $x { $x * 10 }).raku,
   'duckmap descends into a nested Range exactly as into the equivalent List';

# The List forms are unchanged.
is (1, 2, 3, 4).deepmap({ $_ * 2 }).raku, '(2, 4, 6, 8)', 'a List still deepmaps as before';
