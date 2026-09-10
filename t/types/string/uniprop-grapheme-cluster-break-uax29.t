use Test;

# Grapheme_Cluster_Break property (UAX #29). mutsu previously only
# distinguished CR/LF/ZWJ/Extend/Control/Other; verify the full set of
# classes against Rakudo / the Unicode spec.

plan 26;

# Basic
is 'a'.uniprop('Grapheme_Cluster_Break'), 'Other', 'letter is Other';
is "\n".uniprop('Grapheme_Cluster_Break'), 'LF', 'LF is LF';
is "\r".uniprop('Grapheme_Cluster_Break'), 'CR', 'CR is CR';
is "\x200D".uniprop('Grapheme_Cluster_Break'), 'ZWJ', 'ZWJ is ZWJ';
is "\x0301".uniprop('Grapheme_Cluster_Break'), 'Extend', 'combining mark is Extend';
is "\x0009".uniprop('Grapheme_Cluster_Break'), 'Control', 'TAB is Control';

# Regional indicators
is "\x1F1E6".uniprop('Grapheme_Cluster_Break'), 'Regional_Indicator', 'RI A is Regional_Indicator';
is "\x1F1FF".uniprop('Grapheme_Cluster_Break'), 'Regional_Indicator', 'RI Z is Regional_Indicator';

# Hangul jamo
is "\x1100".uniprop('Grapheme_Cluster_Break'), 'L', 'leading jamo is L';
is "\x115F".uniprop('Grapheme_Cluster_Break'), 'L', 'jamo filler is L';
is "\xA960".uniprop('Grapheme_Cluster_Break'), 'L', 'extended-A jamo is L';
is "\x1160".uniprop('Grapheme_Cluster_Break'), 'V', 'vowel jamo is V';
is "\xD7B0".uniprop('Grapheme_Cluster_Break'), 'V', 'extended-B vowel jamo is V';
is "\x11A8".uniprop('Grapheme_Cluster_Break'), 'T', 'trailing jamo is T';
is "\xD7CB".uniprop('Grapheme_Cluster_Break'), 'T', 'extended-B trailing jamo is T';

# Hangul syllables
is "\xAC00".uniprop('Grapheme_Cluster_Break'), 'LV', '가 (LV syllable) is LV';
is "\xAC01".uniprop('Grapheme_Cluster_Break'), 'LVT', '각 (LVT syllable) is LVT';
is "\xD7A3".uniprop('Grapheme_Cluster_Break'), 'LVT', 'last hangul syllable is LVT';

# Prepend
is "\x0600".uniprop('Grapheme_Cluster_Break'), 'Prepend', 'ARABIC NUMBER SIGN is Prepend';
is "\x0D4E".uniprop('Grapheme_Cluster_Break'), 'Prepend', 'MALAYALAM LETTER DOT REPH is Prepend';

# SpacingMark
is "\x0903".uniprop('Grapheme_Cluster_Break'), 'SpacingMark', 'DEVANAGARI SIGN VISARGA is SpacingMark';
is "\x0E33".uniprop('Grapheme_Cluster_Break'), 'SpacingMark', 'THAI SARA AM is SpacingMark';
is "\x0EB3".uniprop('Grapheme_Cluster_Break'), 'SpacingMark', 'LAO SARA AM is SpacingMark';

# SpacingMark exceptions (spacing marks that are NOT SpacingMark)
is "\x102B".uniprop('Grapheme_Cluster_Break'), 'Other', 'MYANMAR VOWEL SIGN TALL AA is Other';
is "\x1A61".uniprop('Grapheme_Cluster_Break'), 'Other', 'TAI THAM VOWEL SIGN A is Other';

# Format
is "\xAD".uniprop('Grapheme_Cluster_Break'), 'Control', 'soft hyphen (Cf) is Control';
