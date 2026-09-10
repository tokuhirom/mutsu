use v6;
use Test;

# `.uninames`, `.uniprops`, `.univals` (and the `uninames` sub form) return a
# Seq in raku, not a List — this is observable via `.WHAT` and `.raku`
# (`("...").Seq`). raku-doc/doc/Type/Cool.rakudoc.

plan 9;

is "abc".uninames.^name, 'Seq', '.uninames returns a Seq';
is "abc".uniprops.^name, 'Seq', '.uniprops returns a Seq';
is "12".univals.^name,   'Seq', '.univals returns a Seq';
is uninames("ab").^name, 'Seq', 'uninames() sub form returns a Seq';

# `.raku` renders the `.Seq` suffix.
is "a".uninames.raku, '("LATIN SMALL LETTER A",).Seq', '.uninames.raku shows .Seq';

# Empty invocant still yields an (empty) Seq, not a List.
is "".uninames.^name, 'Seq', 'empty .uninames is still a Seq';
is "".uniprops.^name, 'Seq', 'empty .uniprops is still a Seq';

# Values are unchanged, only the container type.
is-deeply "AB".uninames.List,
    ("LATIN CAPITAL LETTER A", "LATIN CAPITAL LETTER B").List,
    '.uninames values are correct';
is "½".univals.List, (0.5,).List, '.univals value is correct';
