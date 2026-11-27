use Test;
use nqp;

# A `Uni` is a `uint32` VMArray in rakudo, and mutsu stores it as one: a packed
# codepoint buffer with a start offset (#8673). nqp code edits a Uni in place
# with the positional ops, so each of them has to reach that store, and every
# holder of the Uni has to see the edit. Every expected value here is
# rakudo 2026.07's.

plan 23;

my $u := nqp::strtocodes("héllo", nqp::const::NORMALIZE_NFD, nqp::create(NFD));
is nqp::elems($u), 6, 'strtocodes fills the store with the NFD codepoints';
is nqp::shift_i($u), 104, 'shift_i takes the first codepoint';
is nqp::atpos_i($u, 0) ~ ' ' ~ nqp::atpos_i($u, -1), '101 111',
  'atpos_i reads after a shift, a negative index from the end';
nqp::push_i($u, 33);
nqp::bindpos_i($u, 1, 0x301);
is nqp::strfromcodes($u), "e\x[301]llo!", 'push_i appends, bindpos_i overwrites';
is nqp::pop_i($u), 33, 'pop_i takes the last codepoint';
nqp::splice($u, nqp::list_i(65, 66), 1, 1);
is nqp::strfromcodes($u), 'eABllo', 'splice replaces a codepoint with a list';
nqp::unshift_i($u, 90);
is nqp::strfromcodes($u), 'ZeABllo', 'unshift_i inserts at the front';
is nqp::elems($u), 7, 'elems follows every edit';
nqp::setelems($u, 2);
is nqp::strfromcodes($u), 'Ze', 'setelems truncates';
nqp::bindpos_i($u, 4, 99);
is nqp::elems($u) ~ ' ' ~ nqp::atpos_i($u, 3), '5 0',
  'bindpos_i past the end grows the store with zeros';

my $built := nqp::create(Uni);
nqp::push_i($built, $_) for 97..100;
is nqp::strfromcodes($built), 'abcd', 'a Uni built from nqp::create(Uni)';
is $built.^name, 'Uni', 'it is still a Uni';

my $alias := $built;
nqp::push_i($alias, 101);
is nqp::elems($built), 5, 'an edit through one holder shows in the other';

my $wrapped := nqp::create(Uni);
nqp::push_i($wrapped, -1);
is nqp::atpos_i($wrapped, 0), 4294967295, 'a stored int keeps its low 32 bits';

is "café".NFD.list.join(','), '99,97,102,101,769', 'NFD lists its codepoints';
is "café".NFC.elems, 4, 'NFC counts its codepoints';
is "café".NFD.Str, 'café', 'a Uni stringifies, composed';

my $drain := "abc".NFC;
my $copy := nqp::create(Uni);
nqp::while(nqp::elems($drain), nqp::push_i($copy, nqp::shift_i($drain)));
is nqp::elems($drain) ~ ' ' ~ nqp::strfromcodes($copy), '0 abc',
  'a shift_i/push_i copy loop drains one Uni into another';

my $escape := "a\"b".NFD;
nqp::splice($escape, nqp::list_i(92, 34), 1, 1);
is nqp::strfromcodes($escape), 'a\\"b', 'the escaper splice shape (JSON::Fast str-escape)';

is nqp::atpos_i(nqp::create(Uni), 3), 0, 'reading past the end of an empty Uni gives 0';

# A store filled one push_i at a time grows past its starting capacity, and
# strfromcodes takes an all-ASCII store and a mixed one alike.
my $long := nqp::strtocodes('https://example.org/' x 10, nqp::const::NORMALIZE_NFD, nqp::create(NFD));
my $out := nqp::create(Uni);
nqp::while(nqp::elems($long), nqp::push_i($out, nqp::shift_i($long)));
is nqp::elems($out) ~ ' ' ~ (nqp::strfromcodes($out) eq 'https://example.org/' x 10), '200 True',
  'a 200-codepoint copy loop keeps every codepoint';
my $mixed := nqp::create(Uni);
nqp::push_i($mixed, $_) for 0x61, 0xE9, 0x1F600, 0x7A;
is nqp::strfromcodes($mixed).ords.join(','), '97,233,128512,122',
  'strfromcodes of a store with non-ASCII codepoints';
my $ascii := nqp::strtocodes('a/b-c', nqp::const::NORMALIZE_NFD, nqp::create(NFD));
is nqp::elems($ascii) ~ ' ' ~ nqp::strfromcodes($ascii), '5 a/b-c',
  'strtocodes and strfromcodes round-trip an ASCII string';
