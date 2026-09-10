use Test;

# sprintf numeric directives (%d, %x, %o, %b, %f) given a string argument must
# numify it the Raku way, honouring radix prefixes (0x/0o/0b/0d) and
# underscores. Previously the string was parsed with a plain integer parser, so
# "0x1F" became 0.

plan 14;

is sprintf("%d", "0x1F"), '31', '%d parses hex string';
is sprintf("%d", "0b101"), '5', '%d parses binary string';
is sprintf("%d", "0o17"), '15', '%d parses octal string';
is sprintf("%d", "0d99"), '99', '%d parses 0d decimal string';
is sprintf("%d", "-0x1F"), '-31', '%d parses negative hex string';
is sprintf("%x", "0x1F"), '1f', '%x parses hex string';
is sprintf("%o", "0o17"), '17', '%o parses octal string';
is sprintf("%b", "0b101"), '101', '%b parses binary string';
is sprintf("%f", "0x10"), '16.000000', '%f parses hex string';

# Plain decimal strings and numbers unchanged.
is sprintf("%d", "42"), '42', '%d plain decimal string';
is sprintf("%d", "1_000"), '1000', '%d underscore-separated string';
is sprintf("%d", "3.9"), '3', '%d truncates decimal string';
is sprintf("%d", 255), '255', '%d integer argument';
is sprintf("%x", 255), 'ff', '%x integer argument';
