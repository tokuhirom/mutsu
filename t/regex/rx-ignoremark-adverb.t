use v6;
use Test;

# `rx:ignoremark` / `rx:m` must fold the mark-insensitivity into the compiled
# Regex, exactly like the `m:ignoremark` form does. Regression: `:ignoremark`
# was not in the set of adverbs that force inline-modifier folding, so the
# `rx//` plain-Regex path dropped it and matched mark-sensitively.
# (Language/regexes.rakudoc)

plan 8;

nok (so 'a' ~~ rx/ä/),               'baseline: rx/ä/ does not match "a"';
ok  (so 'a' ~~ rx:ignoremark /ä/),   'rx:ignoremark /ä/ matches "a" (mark stripped from pattern)';
ok  (so 'ä' ~~ rx:ignoremark /a/),   'rx:ignoremark /a/ matches "ä" (mark stripped from text)';
ok  (so 'ỡ' ~~ rx:ignoremark /o/),   'rx:ignoremark strips a decomposed grapheme';
ok  (so 'a' ~~ rx:m /ä/),            'the :m short form works the same';

# The other compile-time inline adverbs on rx// still work (unaffected).
ok  (so 'ABC' ~~ rx:i /abc/),        'rx:i is case-insensitive';
ok  (so 'a b' ~~ rx:s /a b/),        'rx:s honors significant space';
nok (so 'A' ~~ rx/abc/),             'a plain rx// with no adverb is literal';
