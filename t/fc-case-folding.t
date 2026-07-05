use Test;

# .fc (Unicode full case folding). The previous implementation applied an NFKD
# decomposition, which wrongly folded non-cased compatibility characters (NBSP,
# superscripts, ordinals, Roman numerals, circled/fullwidth letters) to their
# decomposition, and missed simple folds that differ from lowercase (final
# sigma, Greek symbol variants, Cherokee).

plan 22;

# Compatibility characters keep their (lowercase) form — no ASCII decomposition
is "Ⅻ".fc, "ⅻ", 'Roman numeral folds to lowercase Roman numeral';
is "Ⅰ".fc, "ⅰ", 'Roman numeral one';
is "Ⓐ".fc, "ⓐ", 'circled A folds to circled a';
is "Ａ".fc, "ａ", 'fullwidth A folds to fullwidth a';
is "²".fc, "²", 'superscript two is unchanged';
is "ª".fc, "ª", 'feminine ordinal is unchanged';
is "\xA0".fc, "\xA0", 'NBSP is unchanged (not folded to space)';
is "\xA8".fc, "\xA8", 'diaeresis is unchanged';

# Ligatures and ß still expand (full fold)
is "ﬀ".fc, "ff", 'ff ligature expands';
is "ﬃ".fc, "ffi", 'ffi ligature expands';
is "ß".fc, "ss", 'sharp s expands';
is "ẞ".fc, "ss", 'capital sharp s expands';

# Simple folds that differ from lowercase
is "ς".fc, "σ", 'final sigma folds to sigma';
is "ſ".fc, "s", 'long s folds to s';
is "ϐ".fc, "β", 'Greek beta symbol folds to beta';
is "µ".fc, "μ", 'micro sign folds to Greek mu';

# Greek iota-subscript
is "ᾳ".fc, "\x3B1\x3B9", 'alpha-with-ypogegrammeni folds to alpha + iota';

# Normal case folding still works
is "HELLO".fc, "hello", 'ASCII uppercase folds';
is "Café".fc, "café", 'accented text folds';
is "İ".fc, "i\x307", 'dotted capital I folds';

# Case-insensitive comparison via fc
ok "STRASSE".fc eq "straße".fc, 'STRASSE and straße fold equal';
ok "ẞ".fc eq "SS".fc, 'capital sharp s and SS fold equal';
