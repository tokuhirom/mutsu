use Test;

# `@array.map(-> $a, $b? { ... })` — a map block whose trailing positional
# parameter is optional. The last chunk, when the source has an odd number
# of elements, has only enough items for the mandatory leading parameter(s);
# the optional trailing one binds to its default (Any) instead of raising an
# arity error. rakudo accepts this without complaint.
#
# Found via HexDump::Tiny's own `hexdump` implementation, which pairs up a
# Blob's bytes two at a time this way to render each hex byte:
#   @v.map(-> $a, $b? { $b ?? sprintf("%02x%02x", $a, $b) !! sprintf("%02x", $a) })
#
# This only reproduced when the source was an Array VARIABLE (`my @v = ...`),
# not a List literal: only the Array case routes through the rw-capable map
# loop (`eval_map_over_items_rw` in src/runtime/resolution_map_grep_rw.rs),
# whose own "requires full binding" branch reimplemented the chunking with a
# fixed arity and no allowance for a short final chunk.

plan 6;

my @v1 = (1);
is-deeply @v1.map(-> $a, $b? { $b ?? "$a,$b" !! "$a" }).List, ("1",).List,
    'single element: optional trailing param unbound';

my @v2 = (1, 2);
is-deeply @v2.map(-> $a, $b? { $b ?? "$a,$b" !! "$a" }).List, ("1,2",).List,
    'two elements: one full chunk';

my @v3 = (1, 2, 3);
is-deeply @v3.map(-> $a, $b? { $b ?? "$a,$b" !! "$a" }).List, ("1,2", "3").List,
    'three elements: one full chunk, one short chunk';

my @v5 = (1, 2, 3, 4, 5);
is-deeply @v5.map(-> $a, $b? { $b ?? "$a,$b" !! "$a" }).List,
    ("1,2", "3,4", "5").List,
    'five elements: two full chunks, one short chunk';

# The HexDump::Tiny idiom itself: hex-encode bytes two at a time.
my @bytes = "foo".encode.list;
is @bytes.map(-> $a, $b? {
    $b ?? sprintf("%02x%02x", $a, $b) !! sprintf("%02x", $a)
}).join(" "), "666f 6f", 'hex-pairs idiom (HexDump::Tiny)';

# A block whose trailing param is still MANDATORY must keep raising on a
# short final chunk — only an optional/defaulted trailing param is forgiven.
dies-ok { my @v = (1, 2, 3); @v.map(-> $a, $b { "$a,$b" }).eager },
    'mandatory trailing param still errors on a short final chunk';

done-testing;
