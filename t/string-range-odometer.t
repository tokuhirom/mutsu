use Test;

# Equal-length inclusive string ranges use Raku's per-position "odometer":
# each character position independently ranges from a[i] to b[i] (ascending if
# a[i] <= b[i], else descending), forming a mixed-radix counter with the
# rightmost position varying fastest. mutsu previously used a string-succ
# sequence, giving e.g. "r2".."t3" the wrong (r2 r3 r4 ... t3) expansion.
# See raku-doc/doc/Language/traps.rakudoc and the QA doc-diff campaign.

plan 12;

is ~("r2".."t3"), 'r2 r3 s2 s3 t2 t3', 'r2..t3 is an odometer, not succ';
is ~("a1".."c3"), 'a1 a2 a3 b1 b2 b3 c1 c2 c3', 'a1..c3 odometer';
is ("az".."bc").elems, 48, 'az..bc: pos1 z..c descends (2 x 24)';
is ~("19".."31"), '19 18 17 16 15 14 13 12 11 29 28 27 26 25 24 23 22 21 39 38 37 36 35 34 33 32 31',
    '19..31: descending digit wheel';
is ~("aa1".."bb3"), 'aa1 aa2 aa3 ab1 ab2 ab3 ba1 ba2 ba3 bb1 bb2 bb3', 'three-position odometer';
is ~("a2".."a5"), 'a2 a3 a4 a5', 'fixed first position';
is ~("ac".."ca"), 'ac ab aa bc bb ba cc cb ca', 'mixed ascending/descending wheels';

# A range whose start sorts after its end is empty.
is ("ba".."ab").elems, 0, 'ba..ab is empty (start after end)';

# Cases where the odometer coincides with string succession stay correct.
is ~("aa".."ad"), 'aa ab ac ad', 'common-prefix range (matches succ)';
is ("aa".."zz").elems, 676, 'aa..zz full 26x26 grid';

# Single-char and different-length ranges are unaffected.
is ~("a".."e"), 'a b c d e', 'single-char range unchanged';
is ~("Y".."AB"), '', 'different-length range stays empty';
