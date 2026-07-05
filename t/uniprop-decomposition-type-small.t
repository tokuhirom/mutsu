use Test;

# The CJK Small Form Variants block (U+FE50..U+FE6F) decomposes with a
# <small> compatibility tag, so its Decomposition_Type is Small. mutsu
# previously fell through to Compat.

plan 8;

is "\xFE50".uniprop('Decomposition_Type'), 'Small', 'SMALL COMMA is Small';
is "\xFE52".uniprop('Decomposition_Type'), 'Small', 'SMALL FULL STOP is Small';
is "\xFE54".uniprop('Decomposition_Type'), 'Small', 'SMALL SEMICOLON is Small';
is "\xFE5F".uniprop('Decomposition_Type'), 'Small', 'SMALL NUMBER SIGN is Small';
is "\xFE6B".uniprop('Decomposition_Type'), 'Small', 'SMALL COMMERCIAL AT is Small';

# Reserved holes in the block have no decomposition
is "\xFE6F".uniprop('Decomposition_Type'), 'None', 'reserved codepoint is None';

# Neighbouring CJK compat vertical forms are unaffected
is "\xFE30".uniprop('Decomposition_Type'), 'Vertical', 'vertical two dot leader is Vertical';
is "\xFF01".uniprop('Decomposition_Type'), 'Wide', 'fullwidth exclamation is Wide';
