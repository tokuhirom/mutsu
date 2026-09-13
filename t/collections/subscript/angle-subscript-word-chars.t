use v6;
use Test;

# Regression (#7954, the `expected statement ...` parse-failure index): an angle
# subscript is a Q-style WORD QUOTE, and rakudo validates nothing inside one —
# it splits on whitespace and every other character is an ordinary member of a
# word. mutsu instead checked each key against an allowlist of key characters
# that had been extended one character at a time (`=`, `(`, `)`, `#`, ... each
# for one distribution), so a key holding anything else failed to parse at all
# (BigRoot: `Hash<RootNumber, FatRat, Natural>.new`).

plan 16;

my %h;

# The construct the index reduced to: commas are ordinary word characters, so
# the subscript is a slice whose keys carry the commas.
is %h<a, b>.elems, 2, 'a comma separates nothing: <a, b> is a two-key slice';
is-deeply (%h<a, b>.map({ .defined }).List), (False, False),
    'both keys of <a, b> miss an empty hash';
my %commas = 'a,' => 1, 'b' => 2;
is-deeply (%commas<a, b>.List), (1, 2), 'and the keys really are "a," and "b"';

# A whole spread of punctuation that the old allowlist rejected.
my %punct = 'a|b' => 'pipe', 'a~b' => 'tilde', 'a^b' => 'caret',
            '[a]' => 'square', '{a}' => 'curly', 'a"b' => 'quote',
            'a;b' => 'semi', 'a\\b' => 'backslash';
is %punct<a|b>, 'pipe', 'a key may hold |';
is %punct<a~b>, 'tilde', 'a key may hold ~';
is %punct<a^b>, 'caret', 'a key may hold ^';
is %punct<[a]>, 'square', 'a key may hold [ ]';
is %punct<{a}>, 'curly', 'a key may hold { }';
is %punct<a"b>, 'quote', 'a key may hold a double quote';
is %punct<a;b>, 'semi', 'a key may hold ;';

# Quotes are not honoured inside an angle subscript, exactly as they are not
# inside a standalone `< ... >` word list, so a quoted pair is two keys.
is %h<'a b'>.elems, 2, q{<'a b'> is two keys, not one quoted key};

# The ordinary forms are unaffected.
my %plain = a => 1, b => 2;
is %plain<a>, 1, 'a single-word key still works';
is-deeply (%plain<a b>.List), (1, 2), 'a whitespace-separated slice still works';
%plain<c> = 3;
is %plain<c>, 3, 'an angle subscript is still an lvalue';

# A spaced `<` after a term is still the comparison operator, not a subscript.
ok (1 < 2), 'a spaced < is still infix less-than';
is-deeply (((%plain,).map({ .<a> })).List), (1,),
    'a hyper-ish nested subscript still resolves';
