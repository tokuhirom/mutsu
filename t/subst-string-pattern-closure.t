use Test;

plan 12;

# `.subst` with a *string* (literal) pattern and a Callable replacement used to
# drop the match (replacing with the empty string) because the string-pattern
# path never invoked the closure. It must call the closure with the match, like
# the regex-pattern path.

is "abcabc".subst("a", *.uc, :g),        'AbcAbc',   'WhateverCode replacement, :g';
is "abc".subst("b", *.uc),               'aBc',      'WhateverCode replacement, single';
is "hello".subst("l", *.uc, :g),         'heLLo',    'WhateverCode replacement over repeats';
is "abcabc".subst("a", { "X" }, :g),     'XbcXbc',   'block replacement ignoring the match';
is "abcabc".subst("a", { $_.uc }, :g),   'AbcAbc',   'block replacement using $_';
is "abcabc".subst("a", -> $m { $m.uc }, :g), 'AbcAbc', 'pointy block replacement binds its parameter';
is "hello world".subst("o", { "0" }, :g), 'hell0 w0rld', 'block replacement, multiple matches';

# a plain string replacement still works
is "aaa".subst("a", "b", :g),            'bbb',      'plain string replacement, :g';
is "abc".subst("a", "X"),                'Xbc',      'plain string replacement, single';

# no match leaves the string unchanged
is "test".subst("x", *.uc),              'test',     'no match leaves the string unchanged';

# the pointy-parameter fix also applies to the regex-pattern path
is "abcabc".subst(/a/, -> $m { $m.uc }, :g), 'AbcAbc', 'regex pattern with pointy replacement';
is "a1b2".subst(/(\d)/, -> $m { $m[0] + 10 }, :g), 'a11b12', 'regex pointy replacement sees captures';
