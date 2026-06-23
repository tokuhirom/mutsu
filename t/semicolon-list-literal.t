use Test;

plan 22;

# A top-level `;` inside `(...)` or `[...]` sections the list: each
# semicolon-group becomes one element — a multi-item group is a sub-list, a
# single-item group is the bare item. A single overall section is not wrapped.

# --- Parenthesized lists ---
is-deeply (1,2;3,4), ((1,2),(3,4)), 'paren: two multi-item sections';
is (1,2;3,4).elems, 2, 'paren: section count';
is-deeply (1,2;3,4;5,6), ((1,2),(3,4),(5,6)), 'paren: three sections';
is-deeply (1,2;3), ((1,2),3), 'paren: mixed multi/single section';
is-deeply (1;2,3), (1,(2,3)), 'paren: single then multi';
is-deeply (1;2;3), (1,2,3), 'paren: all single-item sections flatten';
is-deeply (1,2,3;), (1,2,3), 'paren: trailing semicolon, one section stays flat';
is (1;), 1, 'paren: single item with trailing semicolon is bare';
is-deeply (1,2;), (1,2), 'paren: one multi-item section with trailing semicolon';
is-deeply (1..2;3..4), (1..2,3..4), 'paren: single-element Range sections';
is (1,2;3,4).raku, '((1, 2), (3, 4))', 'paren: .raku nests';
is-deeply (1,2;3,4)[1], (3,4), 'paren: subscript selects a section';
is (1,2;3,4)[1][0], 3, 'paren: nested subscript';

# Assignment preserves the nesting.
my @a = (1,2;3,4);
is @a.elems, 2, 'assigned array has 2 rows';
is @a[1].sum, 7, 'second row sums correctly';

# --- Array composers ---
is-deeply [1,2;3,4], [(1,2),(3,4)], 'array: two multi-item sections';
is [1,2;3,4].elems, 2, 'array: section count';
is-deeply [1;2;3], [1,2,3], 'array: all single-item sections flatten';
is-deeply [1,2,3;], [1,2,3], 'array: trailing semicolon stays flat';
is-deeply [1,2;3], [(1,2),3], 'array: mixed sections';
is-deeply [1;], [1], 'array: single item trailing semicolon is a 1-elem array';
is [1,2;3,4].raku, '[(1, 2), (3, 4)]', 'array: .raku nests';
