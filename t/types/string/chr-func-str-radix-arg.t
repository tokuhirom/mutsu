use v6.d;
use Test;

# From the TOML distribution: its \u escape decoder calls
# chr(":16<{substr(...)}>"), a Str in Raku's radix-literal syntax.

plan 6;

is chr("65"), 'A', 'decimal string';
is chr("0x41"), 'A', 'hex prefix string';
is chr(":16<41>"), 'A', ':16<..> string';
is chr(":2<1000001>"), 'A', ':2<..> string';
is chr(":16<00e9>"), 'é', 'non-ASCII codepoint';
is ":16<41>".chr, chr(":16<41>"), 'method form agrees';

done-testing;
