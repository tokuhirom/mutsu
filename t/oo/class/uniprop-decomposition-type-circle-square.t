use Test;

# Decomposition_Type Circle / Square / Wide / Vertical sets (UAX #44). mutsu
# used over-broad block ranges (e.g. all of U+2460..U+24FF as Circle and only
# U+3300..U+33FF as Square), so parenthesized forms were mis-tagged Circle and
# many enclosed/squared CJK symbols were reported as Compat.

plan 18;

# Circle (enclosed alphanumerics/ideographs)
is "\x2460".uniprop('Decomposition_Type'), 'Circle', 'circled one is Circle';
is "\x24B6".uniprop('Decomposition_Type'), 'Circle', 'circled A is Circle';
is "\x3251".uniprop('Decomposition_Type'), 'Circle', 'circled 21 is Circle';
is "\x32D0".uniprop('Decomposition_Type'), 'Circle', 'circled katakana a is Circle';
is "\x1F250".uniprop('Decomposition_Type'), 'Circle', 'circled ideograph is Circle';

# Parenthesized / period forms are NOT Circle
is "\x2474".uniprop('Decomposition_Type'), 'Compat', 'parenthesized one is Compat';
is "\x2488".uniprop('Decomposition_Type'), 'Compat', 'digit-one-full-stop is Compat';

# Square (squared CJK/Latin abbreviations)
is "\x3357".uniprop('Decomposition_Type'), 'Square', 'squared apaato is Square';
is "\x1F130".uniprop('Decomposition_Type'), 'Square', 'squared latin A is Square';
is "\x1F200".uniprop('Decomposition_Type'), 'Square', 'squared hiragana is Square';

# Wide (fullwidth)
is "\xFF01".uniprop('Decomposition_Type'), 'Wide', 'fullwidth exclamation is Wide';
is "\xFF5F".uniprop('Decomposition_Type'), 'Wide', 'fullwidth white paren is Wide';
is "\x3000".uniprop('Decomposition_Type'), 'Wide', 'ideographic space is Wide';

# Vertical (CJK compatibility vertical forms)
is "\xFE10".uniprop('Decomposition_Type'), 'Vertical', 'presentation comma is Vertical';
is "\xFE30".uniprop('Decomposition_Type'), 'Vertical', 'vertical two dot leader is Vertical';
is "\x30FF".uniprop('Decomposition_Type'), 'Vertical', 'katakana digraph koto is Vertical';

# Halfwidth still Narrow
is "\xFF61".uniprop('Decomposition_Type'), 'Narrow', 'halfwidth ideographic stop is Narrow';
is "\xFFE8".uniprop('Decomposition_Type'), 'Narrow', 'halfwidth forms light vertical is Narrow';
