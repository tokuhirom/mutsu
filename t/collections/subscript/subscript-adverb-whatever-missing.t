use Test;

plan 12;

my @a = <a b c d>;

# A WhateverCode subscript (`*-1`) carrying an adverb resolves the index against
# the array, exactly like the plain-value subscript `@a[*-1]`.
is-deeply (@a[*-1]:k),  3,            'WhateverCode index :k → resolved index';
is-deeply (@a[*-1]:v),  "d",          'WhateverCode index :v → element';
is-deeply (@a[*-1]:kv), (3, "d"),     'WhateverCode index :kv';
is-deeply (@a[*-1]:p),  (3 => "d"),   'WhateverCode index :p';
is-deeply (@a[*-2]:k),  2,            'WhateverCode *-2 :k';
is-deeply (@a[*-1, *-2]:k), (3, 2),   'WhateverCode slice :k';

# Missing-element keep_missing default reads the array's element type from the
# container value (survives binding), not just the variable's declared type.
my Str @s = <a b c d>;
is-deeply (@s[11]:!v),  Str,          'typed-array missing :!v → element type';
is-deeply (@s[11]:!kv), (11, Str),    'typed-array missing :!kv';
is-deeply (@s[11]:!p),  (11 => Str),  'typed-array missing :!p';

# An untyped array's missing default is Any.
is-deeply (@a[11]:!v),  Any,          'untyped-array missing :!v → Any';

# The same holds when the typed array is the bound loop variable.
for $@s, Str -> @b, $T {
    is-deeply (@b[11]:!v), Str,       'bound typed-array missing :!v → element type';
    is $T, Str,                       'loop type param bound';
}
