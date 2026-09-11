use lib 't/lib';
use Test;

# `is export(:TAG)` on an enum must register every enum value under the named
# tag, just like the same trait on a sub. Pin the three declaration forms from
# the issue and keep the exported sub as the control case.
use EnumTaggedExportFixture :PlainTag;
import EnumTaggedExportFixture :BareTag;
import EnumTaggedExportFixture :TypedTag;
import EnumTaggedExportFixture :SubTag;

enum PairBody is export (PAIR_FOUND => 100, PAIR_GONE => 410);

plan 9;

is tagged-control(), 'sub', 'a tagged sub remains importable';
is A.key, 'A', 'an `our` enum value imports through its named tag';
is B.key, 'B', 'the second `our` enum value imports through its named tag';
is C.key, 'C', 'a bare enum value imports through its named tag';
is D.key, 'D', 'the second bare enum value imports through its named tag';
is E.value, 'e', 'a typed enum value imports through its named tag';
is F.value, 'f', 'the second typed enum value imports through its named tag';
is PAIR_FOUND.value, 100, 'an exported pair-list enum keeps its declaration body';
is PAIR_GONE.value, 410, 'the second exported pair-list enum value is preserved';
