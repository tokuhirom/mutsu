use Test;

# `\c[...]` and `uniparse` resolve CLDR emoji short names through a committed,
# binary-searched table (#10437). Lookup is case-insensitive, and ZWJ-sequence
# names whose CLDR form has commas resolve in their comma-less spelling.

plan 8;

is "\c[grinning face]".ord.base(16), '1F600', 'plain emoji name';
is "\c[GRINNING FACE]".ord.base(16), '1F600', 'emoji name is case-insensitive';
is "\c[family: man woman girl boy]".ords.map(*.base(16)).join(' '),
    '1F468 200D 1F469 200D 1F467 200D 1F466',
    'ZWJ family sequence, comma-less spelling';
is "\c[family: man woman girl boy]".chars, 1, 'ZWJ sequence is one grapheme';
is uniparse('woman gesturing OK').ords.join(' '), '128582 8205 9792 65039',
    'uniparse resolves a ZWJ emoji name';
is uniparse('Woman Gesturing OK').ords.join(' '), '128582 8205 9792 65039',
    'uniparse emoji name is case-insensitive';
is "\c[LATIN SMALL LETTER A]", 'a', 'a plain Unicode name still wins';
is "\c[woman technologist]\c[rainbow flag]".ords.map(*.base(16)).join(' '),
    '1F469 200D 1F4BB 1F3F3 FE0F 200D 1F308', 'two emoji sequence names in a row';
