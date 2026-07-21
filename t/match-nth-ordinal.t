use Test;

# `.match` accepts the ordinal adverb shortcuts :1st/:2nd/:3rd/:Nth, which
# parse as `st => 1`, `nd => 2`, `rd => 3`, `th => N` and are equivalent to
# :nth(N) (see Type/Str.rakudoc match examples).

plan 8;

is "foo[bar][baz]".match(/../, :1st), 'fo', ':1st selects the first match';
is "foo[bar][baz]".match(/../, :2nd), 'o[', ':2nd selects the second match';
is "foo[bar][baz]".match(/../, :3rd), 'ba', ':3rd selects the third match';
is "foo[bar][baz]".match(/../, :4th), 'r]', ':4th selects the fourth match';

# Two-digit ordinal via :th(N)
is "aaaaaaaaaaaa".match(/a/, :12th), 'a', ':12th selects the twelfth match';

# Equivalence with :nth(N)
is "foo[bar][baz]".match(/../, :2nd),
   "foo[bar][baz]".match(/../, :nth(2)),
   ':2nd matches :nth(2)';

# Out-of-range ordinal yields no match
is "ab".match(/./, :5th), Nil, 'out-of-range ordinal returns Nil';

# Ordinal shortcut still cooperates with a preceding literal-count position
is "aXaXaX".match(/a/, :3rd), 'a', ':3rd across separated matches';
