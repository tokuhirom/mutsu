use Test;
use nqp;

# The `nqp::` text surface: character classes, Unicode properties, codepoint
# arrays and the string/hash primitives (runtime/nqp_ops_text.rs and
# runtime/nqp_ops_str.rs). Every number asserted here is MoarVM's, measured
# against rakudo -- nqp code branches on the raw integers, so a self-consistent
# numbering of our own would run such code silently wrong. Found by making
# String::Utils's test suite run, which is written almost entirely in these ops.

plan 46;

#- character classes -----------------------------------------------------------

is nqp::iscclass(nqp::const::CCLASS_UPPERCASE, "A", 0), 1, 'A is uppercase';
is nqp::iscclass(nqp::const::CCLASS_UPPERCASE, "a", 0), 0, 'a is not uppercase';
is nqp::iscclass(nqp::const::CCLASS_LOWERCASE, "a", 0), 1, 'a is lowercase';
is nqp::iscclass(nqp::const::CCLASS_NUMERIC, "7", 0),   1, '7 is numeric';
is nqp::iscclass(nqp::const::CCLASS_NUMERIC, "\xBC", 0), 0,
  'VULGAR FRACTION ONE QUARTER is No, not numeric (gc Nd, not the Numeric property)';
is nqp::iscclass(nqp::const::CCLASS_ALPHABETIC, "\x2160", 0), 0,
  'ROMAN NUMERAL ONE is Nl, so not CCLASS_ALPHABETIC (gc L*, not the Alphabetic property)';
is nqp::iscclass(nqp::const::CCLASS_HEXADECIMAL, "f", 0), 1, 'f is a hex digit';
is nqp::iscclass(nqp::const::CCLASS_HEXADECIMAL, "g", 0), 0, 'g is not a hex digit';
is nqp::iscclass(nqp::const::CCLASS_WHITESPACE, "\xA0", 0), 1, 'NBSP is whitespace';
is nqp::iscclass(nqp::const::CCLASS_BLANK, "\t", 0),    1, 'tab is blank';
is nqp::iscclass(nqp::const::CCLASS_BLANK, "\n", 0),    0, 'newline is not blank';
is nqp::iscclass(nqp::const::CCLASS_NEWLINE, "\n", 0),  1, 'newline is a newline';
is nqp::iscclass(nqp::const::CCLASS_CONTROL, "\t", 0),  1, 'tab is a control character';
is nqp::iscclass(nqp::const::CCLASS_PRINTING, "\t", 0), 0, 'tab does not print';
is nqp::iscclass(nqp::const::CCLASS_PRINTING, "\xAD", 0), 1,
  'SOFT HYPHEN is Cf, and everything that is not Cc prints';
is nqp::iscclass(nqp::const::CCLASS_PUNCTUATION, "_", 0), 1, 'underscore is punctuation (Pc)';
is nqp::iscclass(nqp::const::CCLASS_PUNCTUATION, "+", 0), 0, 'plus is a symbol, not punctuation';
is nqp::iscclass(nqp::const::CCLASS_WORD, "_", 0),      1, 'underscore is a word character';
is nqp::iscclass(nqp::const::CCLASS_WORD, "-", 0),      0, 'hyphen is not a word character';

# findcclass / findnotcclass answer the END of the window when there is no
# match, not -1 -- which is what lets `findnotcclass(...) == chars($s)` mean
# "the whole string is of this class".
is nqp::findnotcclass(nqp::const::CCLASS_NUMERIC, "123", 0, 3), 3,
  'findnotcclass runs off the end of an all-numeric string';
is nqp::findnotcclass(nqp::const::CCLASS_NUMERIC, "12a", 0, 3), 2,
  'findnotcclass finds the first non-member';
is nqp::findcclass(nqp::const::CCLASS_WORD, "  ab", 0, 4), 2,
  'findcclass finds the first member';
is nqp::findcclass(nqp::const::CCLASS_WORD, "    ", 0, 4), 4,
  'findcclass runs off the end when there is no member';

#- Unicode properties ----------------------------------------------------------

my $gc = nqp::unipropcode("General_Category");
is $gc, 20, 'the General_Category property code is MoarVM\'s 20';
is nqp::getuniprop_int("a".ord, $gc), 2,  'Ll is value code 2';
is nqp::getuniprop_int("A".ord, $gc), 1,  'Lu is value code 1';
is nqp::getuniprop_int(0x301,   $gc), 6,  'Mn is value code 6 (what "is a mark" tests compare against)';
is nqp::getuniprop_int("0".ord, $gc), 9,  'Nd is value code 9';
is nqp::getuniprop_int(" ".ord, $gc), 12, 'Zs is value code 12';

#- codepoint arrays ------------------------------------------------------------

my $codes := nqp::strtocodes("a\c[COMBINING ACUTE ACCENT]", nqp::const::NORMALIZE_NFC,
                             nqp::create(array[uint32]));
is nqp::elems($codes), 1, 'NFC composes the decomposed pair into one codepoint';
is nqp::atpos_i($codes, 0), 0xE1, '... and it is LATIN SMALL LETTER A WITH ACUTE';

my $nfd := nqp::strtocodes("\xE9", nqp::const::NORMALIZE_NFD, nqp::create(array[uint32]));
is nqp::elems($nfd), 2, 'NFD decomposes it back into two';

# The target is REPLACED, not appended to: nqp code allocates one buffer and
# re-fills it per item (String::Utils's `root` does exactly this).
my $reused := nqp::create(array[uint32]);
nqp::strtocodes("ab", nqp::const::NORMALIZE_NFC, $reused);
nqp::strtocodes("xyz", nqp::const::NORMALIZE_NFC, $reused);
is nqp::elems($reused), 3, 'strtocodes replaces the target rather than appending to it';
is nqp::strfromcodes($reused), "xyz", 'strfromcodes turns codepoints back into a string';

#- string primitives -----------------------------------------------------------

is nqp::substr("hello", 2),      "llo", 'substr from an offset';
is nqp::substr("hello", 1, 3),   "ell", 'substr with a length';
is nqp::substr("hello", 3, 99),  "lo",  'substr past the end truncates';
is nqp::substr("h\xE9llo", 1, 2), "\xE9l",
  'substr counts codepoints, not bytes';
is nqp::concat("foo", "bar"), "foobar", 'concat';
is nqp::index("hello", "ll"), 2,  'index finds a substring';
is nqp::index("hello", "zz"), -1, 'index answers -1 when absent, where Raku index answers Nil';
is nqp::eqat("hello", "ell", 1), 1, 'eqat at the right position';
is nqp::eqat("hello", "ell", 2), 0, 'eqat at the wrong position';
is nqp::flip("abc"), "cba", 'flip';
is nqp::x("ab", 3), "ababab", 'x repeats';
is nqp::mod_i(-7, 3), -1, 'mod_i truncates like MoarVM, it does not floor like Raku %';

# vim: expandtab shiftwidth=4
