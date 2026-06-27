use Test;

plan 12;

# A zen slice (empty subscript `@a[]`) carrying a subscript adverb behaves like
# the whatever slice `@a[*]:adverb`: it yields all keys/values/pairs. Previously
# `@a[]:k` dropped the `[]` and mis-parsed `:k` as a colonpair argument.

my @a = <a b c d>;

is-deeply (@a[]:k),  (0, 1, 2, 3),                          'zen slice :k → all keys';
is-deeply (@a[]:v),  ("a", "b", "c", "d"),                  'zen slice :v → all values';
is-deeply (@a[]:kv), (0, "a", 1, "b", 2, "c", 3, "d"),      'zen slice :kv';
is-deeply (@a[]:p),  (0 => "a", 1 => "b", 2 => "c", 3 => "d"), 'zen slice :p';
is-deeply (@a[]:!k), (0, 1, 2, 3),                          'zen slice :!k';
is-deeply (@a[]:!v), ("a", "b", "c", "d"),                  'zen slice :!v';

# Conditional adverb argument.
is-deeply (@a[]:k(True)),  (0, 1, 2, 3),               'zen slice :k(True)';
is-deeply (@a[]:v(True)),  ("a", "b", "c", "d"),       'zen slice :v(True)';

# A bare zen slice (no adverb) is still the identity on the list.
is-deeply @a[], @a,                                          'bare zen slice is identity';

# Matches the whatever slice exactly.
is-deeply (@a[]:k),  (@a[*]:k),  'zen :k matches whatever :k';
is-deeply (@a[]:v),  (@a[*]:v),  'zen :v matches whatever :v';
is-deeply (@a[]:kv), (@a[*]:kv), 'zen :kv matches whatever :kv';
