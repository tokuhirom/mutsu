use Test;

# Str.leading-whitespace / .trailing-whitespace: the whitespace
# trim-leading / trim-trailing drop (rakudo core; its own RakuAST::Deparse
# uses them). Measured on rakudo 2026.09.

plan 6;

is "\t a b".leading-whitespace, "\t ", 'leading whitespace';
is "  a b \n".trailing-whitespace, " \n", 'trailing whitespace';
is "ab".leading-whitespace, '', 'none leading';
is "ab".trailing-whitespace, '', 'none trailing';
is "".leading-whitespace, '', 'an empty string';
is "   ".trailing-whitespace, '   ', 'all whitespace';
