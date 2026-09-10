use Test;

# Mathematical Alphanumeric Symbols and the segmented (seven-segment) digits
# decompose to their ASCII base with a <font> tag, so their
# Decomposition_Type is Font. mutsu previously reported Compat.

plan 12;

# Mathematical Alphanumeric Symbols (U+1D400..U+1D7FF)
is "\x1D400".uniprop('Decomposition_Type'), 'Font', 'MATH BOLD CAPITAL A is Font';
is "\x1D41A".uniprop('Decomposition_Type'), 'Font', 'MATH BOLD SMALL A is Font';
is "\x1D538".uniprop('Decomposition_Type'), 'Font', 'MATH DOUBLE-STRUCK A is Font';
is "\x1D49C".uniprop('Decomposition_Type'), 'Font', 'MATH SCRIPT CAPITAL A is Font';
is "\x1D7CE".uniprop('Decomposition_Type'), 'Font', 'MATH BOLD DIGIT ZERO is Font';
is "\x1D7FF".uniprop('Decomposition_Type'), 'Font', 'MATH MONOSPACE DIGIT NINE is Font';

# Segmented digits (U+1FBF0..U+1FBF9)
is "\x1FBF0".uniprop('Decomposition_Type'), 'Font', 'SEGMENTED DIGIT ZERO is Font';
is "\x1FBF9".uniprop('Decomposition_Type'), 'Font', 'SEGMENTED DIGIT NINE is Font';

# Reserved holes in the math block have no decomposition
is "\x1D455".uniprop('Decomposition_Type'), 'None', 'reserved math codepoint is None';

# Nearby blocks unaffected
is '½'.uniprop('Decomposition_Type'), 'Fraction', 'vulgar fraction still Fraction';
is "\x2070".uniprop('Decomposition_Type'), 'Super', 'superscript still Super';
is 'A'.uniprop('Decomposition_Type'), 'None', 'plain letter is None';
