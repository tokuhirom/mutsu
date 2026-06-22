use Test;

# Allomorphic values (IntStr / RatStr / NumStr) created by `<...>` carry both a
# numeric value and the original source string. `.raku` / `.perl` render them as
# `TypeStr.new(<numeric>, "<string>")`, while `.gist` / `.Str` / `say` use the
# preserved source string.

plan 18;

# --- .raku / .perl show the allomorph constructor form ---
is <42>.raku,    'IntStr.new(42, "42")',       'IntStr .raku';
is <42>.perl,    'IntStr.new(42, "42")',       'IntStr .perl';
is <-5>.raku,    'IntStr.new(-5, "-5")',       'negative IntStr .raku';
is <0>.raku,     'IntStr.new(0, "0")',         'zero IntStr .raku';
is <3.14>.raku,  'RatStr.new(3.14, "3.14")',   'RatStr .raku';
is <1e3>.raku,   'NumStr.new(1000e0, "1e3")',  'NumStr .raku keeps source string';

# --- .gist / .Str / say use the preserved source string, NOT the numeric ---
is <1e3>.gist,   '1e3',  'NumStr .gist is the source string';
is <1e3>.Str,    '1e3',  'NumStr .Str is the source string';
is <42>.gist,    '42',   'IntStr .gist';
is <3.14>.gist,  '3.14', 'RatStr .gist';

# --- inside a list, each allomorph renders independently ---
is <1 2 3>.raku, '(IntStr.new(1, "1"), IntStr.new(2, "2"), IntStr.new(3, "3"))',
    'list of IntStr .raku';
is (<1e3>,).gist, '(1e3)',  'NumStr in a list gists as source string';
is [<1e3>].gist,  '[1e3]',  'NumStr in an array gists as source string';

# --- the WHAT type is preserved ---
is <42>.WHAT.raku,   'IntStr', 'IntStr WHAT';
is <3.14>.WHAT.raku, 'RatStr', 'RatStr WHAT';
is <1e3>.WHAT.raku,  'NumStr', 'NumStr WHAT';

# --- allomorphs still behave numerically / stringily ---
is <42> + 1,   43,    'IntStr numifies';
is <42> ~ "x", '42x', 'IntStr stringifies';
