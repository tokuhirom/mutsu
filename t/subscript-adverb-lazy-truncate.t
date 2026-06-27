use Test;

plan 8;

# A lazy subscript (`@a[lazy 11..12]`) auto-truncates at the array end: indices
# past the end are dropped, not reported as missing — so every adverb yields an
# empty result. An eager out-of-range Range keeps the missing elements.

my Str @a = <a b c d>;

is-deeply  @a[lazy 11..12],      (),   'lazy out-of-range value → empty';
is-deeply (@a[lazy 11..12]:k),   (),   'lazy out-of-range :k → empty';
is-deeply (@a[lazy 11..12]:!k),  (),   'lazy out-of-range :!k → empty (truncated, not missing)';
is-deeply (@a[lazy 11..12]:v),   (),   'lazy out-of-range :v → empty';
is-deeply (@a[lazy 11..12]:!v),  (),   'lazy out-of-range :!v → empty';
is-deeply (@a[lazy 11..12]:kv),  (),   'lazy out-of-range :kv → empty';

# A lazy range that is partly in range keeps only the in-range part.
is-deeply (@a[lazy 2..10]:k),    (2, 3),       'lazy partial range :k keeps in-range indices';
is-deeply (@a[lazy 2..10]:v),    ("c", "d"),   'lazy partial range :v keeps in-range values';
