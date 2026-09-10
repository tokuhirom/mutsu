use v6;
use Test;

# A compact combined character class `<:Ll+:N>` joins Unicode-property (and
# named-class) atoms with top-level `+`/`-` set operators, without the spaced
# `<+:Ll +:N>` form. Regression: mutsu treated the whole `Ll+:N` as one bogus
# property name and never matched. (raku-doc Language/regexes.rakudoc)

plan 9;

ok  ("9" ~~ /<:Ll+:N>/),          '<:Ll+:N> matches a number';
ok  ("a" ~~ /<:Ll+:N>/),          '<:Ll+:N> matches a lowercase letter';
nok ("A" ~~ /<:Ll+:N>/),          '<:Ll+:N> rejects an uppercase letter';
ok  ("9" ~~ /<:N+:Ll>/),          'operand order does not matter';
ok  ("9" ~~ /<:Ll+:Nd>/),         'combined with a two-letter property (:Nd)';

# Set difference: lowercase minus uppercase (uppercase can never be Ll anyway,
# but the `-` operator must parse).
ok  ("x" ~~ /<:Ll-:Lu>/),         '<:Ll-:Lu> set difference parses and matches';

# A plain single property still works (no combine operator).
ok  ("9" ~~ /<:N>/),              'single <:N> still matches';

# The spaced form is unchanged.
ok  ("9" ~~ /<+:Ll +:N>/),        'spaced <+:Ll +:N> still matches';

# The documented example: capture a combined class.
"raku9" ~~ /\w+(<:Ll+:N>)/;
is ~$0, "9",                      'captured <:Ll+:N> from the doc example';
