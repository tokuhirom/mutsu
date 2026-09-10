use Test;

# Decomposition_Type Super / Sub / Font / Nobreak sets (UAX #44). mutsu only
# covered a few superscript/subscript codepoints and reported the rest of
# these ranges as Compat.

plan 20;

# Super
is 'ª'.uniprop('Decomposition_Type'), 'Super', 'feminine ordinal is Super';
is 'º'.uniprop('Decomposition_Type'), 'Super', 'masculine ordinal is Super';
is "\x02B0".uniprop('Decomposition_Type'), 'Super', 'modifier small h is Super';
is "\x2071".uniprop('Decomposition_Type'), 'Super', 'superscript i is Super';
is "\x207F".uniprop('Decomposition_Type'), 'Super', 'superscript n is Super';
is "\x2122".uniprop('Decomposition_Type'), 'Super', 'trade mark sign is Super';

# Sub
is "\x1D62".uniprop('Decomposition_Type'), 'Sub', 'subscript i is Sub';
is "\x2C7C".uniprop('Decomposition_Type'), 'Sub', 'subscript j is Sub';
is "\x2095".uniprop('Decomposition_Type'), 'Sub', 'subscript h is Sub';

# Font (letterlike + Hebrew + Arabic math + math alphanumerics)
is "\x2102".uniprop('Decomposition_Type'), 'Font', 'double-struck C is Font';
is "\x210B".uniprop('Decomposition_Type'), 'Font', 'script H is Font';
is "\xFB20".uniprop('Decomposition_Type'), 'Font', 'Hebrew wide ayin is Font';
is "\x1EE00".uniprop('Decomposition_Type'), 'Font', 'Arabic math alef is Font';
is "\x1D400".uniprop('Decomposition_Type'), 'Font', 'math bold A is Font';

# Nobreak
is "\xA0".uniprop('Decomposition_Type'), 'Nobreak', 'NBSP is Nobreak';
is "\x2011".uniprop('Decomposition_Type'), 'Nobreak', 'non-breaking hyphen is Nobreak';
is "\x0F0C".uniprop('Decomposition_Type'), 'Nobreak', 'Tibetan delimiter is Nobreak';

# Unaffected neighbours
is "\x2160".uniprop('Decomposition_Type'), 'Compat', 'Roman numeral is Compat';
is '½'.uniprop('Decomposition_Type'), 'Fraction', 'vulgar fraction is Fraction';
is 'A'.uniprop('Decomposition_Type'), 'None', 'plain letter is None';
