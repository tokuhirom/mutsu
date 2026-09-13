use v6;
use Test;
use nqp;

# A `Uni` IS its codepoints. rakudo declares it `is repr('VMArray')
# is array_type(uint32)`, and nqp code treats one as exactly that: it indexes a
# Uni with `nqp::atpos_i`, consumes it with `nqp::shift_i`, rewrites it in place
# with `nqp::splice`, and builds one from scratch with `nqp::create(Uni)` plus
# `nqp::push_i`. Upstream `JSON::Fast`'s escaper and its string scanner are both
# written that way (#8226).
#
# mutsu used to store a Uni as the normalized *string*, which left it with no
# element store at all: `nqp::elems` answered 0, so the escaper's scan loop never
# ran a single iteration and `to-json` silently emitted unescaped — invalid —
# JSON. Measured against the `raku` oracle.

plan 14;

# -- a Uni answers about its codepoints ---------------------------------------

my $nfd := "b\c[LATIN SMALL LETTER A WITH RING ABOVE]".NFD;
is nqp::elems($nfd), 3, 'nqp::elems of a Uni is its codepoint count';
is nqp::atpos_i($nfd, 0), 98, 'nqp::atpos_i reads a Uni codepoint';
is nqp::atpos_i($nfd, 1), 97, 'the decomposed base character';
is nqp::atpos_i($nfd, 2), 0x30A, 'the combining ring above';

# -- nqp::create(Uni) is an empty, fillable store ------------------------------

my $built := nqp::create(Uni);
is nqp::elems($built), 0, 'nqp::create(Uni) starts empty';
nqp::push_i($built, 104);
nqp::push_i($built, 105);
is nqp::elems($built), 2, 'nqp::push_i appends to it';
is nqp::strfromcodes($built), "hi", 'nqp::strfromcodes spells what was pushed';

# -- a Uni is CONSUMABLE and REWRITABLE in place ------------------------------

my $codes := nqp::strtocodes("xyz", nqp::const::NORMALIZE_NFD, nqp::create(Uni));
is nqp::shift_i($codes), 120, 'nqp::shift_i takes the first codepoint off a Uni';
is nqp::elems($codes), 2, 'and the Uni is one shorter';

my $escaped := "a\"b".NFD;
# What `str-escape` does: replace the quote with the two codepoints `\` `"`.
nqp::splice($escaped, nqp::list_i(92, 34), 1, 1);
is nqp::strfromcodes($escaped), Q[a\"b], 'nqp::splice rewrites a Uni in place';

# A second holder of the same Uni sees the rewrite, the way two references to a
# VMArray do.
my $alias := $escaped;
nqp::splice($alias, nqp::list_i(33), 0, 1);
is nqp::strfromcodes($escaped), Q[!\"b], 'two holders of one Uni share its store';

# -- strfromcodes normalizes, because a VM string is NFG ----------------------

is nqp::strfromcodes("b\c[LATIN SMALL LETTER A WITH RING ABOVE]".NFD).chars, 2,
    'nqp::strfromcodes composes a decomposed sequence back to graphemes';
is nqp::strfromcodes("b\c[LATIN SMALL LETTER A WITH RING ABOVE]".NFD),
    "b\c[LATIN SMALL LETTER A WITH RING ABOVE]",
    'so round-tripping a string through .NFD and back is the identity';

# -- the Raku-level surface is unchanged --------------------------------------

is "ff".NFKC.gist, 'NFKC:0x<0066 0066>', 'a Uni still gists as its codepoints';

# vim: ft=perl6
