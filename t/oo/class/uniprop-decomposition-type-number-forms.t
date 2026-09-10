use Test;

# Decomposition_Type of Number Forms and related compatibility characters.
# mutsu previously classified the whole U+2150..U+218F block as Fraction
# (so Roman numerals were wrong) and missed the Latin subscript letters and
# the Latin-1 vulgar fractions.

plan 16;

# Vulgar fractions
is '¼'.uniprop('Decomposition_Type'), 'Fraction', 'one quarter is Fraction';
is '½'.uniprop('Decomposition_Type'), 'Fraction', 'one half is Fraction';
is '¾'.uniprop('Decomposition_Type'), 'Fraction', 'three quarters is Fraction';
is "\x2153".uniprop('Decomposition_Type'), 'Fraction', 'one third is Fraction';
is "\x215F".uniprop('Decomposition_Type'), 'Fraction', 'numerator one is Fraction';
is "\x2189".uniprop('Decomposition_Type'), 'Fraction', 'zero thirds is Fraction';

# Roman numerals are Compat, not Fraction
is "\x2160".uniprop('Decomposition_Type'), 'Compat', 'Roman numeral one is Compat';
is "\x216C".uniprop('Decomposition_Type'), 'Compat', 'Roman numeral fifty is Compat';
is "\x2170".uniprop('Decomposition_Type'), 'Compat', 'small roman numeral one is Compat';
is "\x217F".uniprop('Decomposition_Type'), 'Compat', 'small roman numeral M is Compat';

# Latin subscript letters
is "\x2090".uniprop('Decomposition_Type'), 'Sub', 'subscript a is Sub';
is "\x2095".uniprop('Decomposition_Type'), 'Sub', 'subscript h is Sub';
is "\x209C".uniprop('Decomposition_Type'), 'Sub', 'subscript t is Sub';

# Subscript digits still Sub, superscripts still Super
is "\x2080".uniprop('Decomposition_Type'), 'Sub', 'subscript zero is Sub';
is "\x2070".uniprop('Decomposition_Type'), 'Super', 'superscript zero is Super';
is '²'.uniprop('Decomposition_Type'), 'Super', 'superscript two is Super';
