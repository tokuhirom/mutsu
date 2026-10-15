use Test;

# ADR-11276 §9.112: `Cool.Rat`, `Cool.FatRat` and `Any.tree` are rows, and the
# receivers the deleted `methods_0arg/dispatch_core_math.rs` arms answered
# reach them.

plan 29;

# `Range`, `Seq` and `Match` are `Cool`: their `Rat` is their number.
is (1..3).Rat, 3, 'Range.Rat is its element count';
isa-ok (1..3).FatRat, FatRat, 'Range.FatRat';
is (1, 2, 3).Seq.Rat, 3, 'Seq.Rat is its element count';
"x 42" ~~ /\d+/;
is $/.Rat, 42, 'Match.Rat parses its text';

# A `StrDistance` numifies to its edit distance.
my $s = "abc";
my $d = ($s ~~ tr/a/x/);
is $d.Rat, 1, 'StrDistance.Rat';
isa-ok $d.FatRat, FatRat, 'StrDistance.FatRat';
is $d.Numeric, 1, 'StrDistance.Numeric';

# A string still parses, and a non-number is a Failure.
is "1.5".Rat, 1.5, 'Str.Rat';
isa-ok "1/3".FatRat, FatRat, 'Str.FatRat';
fails-like { "abc".Rat }, X::Str::Numeric, 'Str.Rat of a non-number';

# `Any.tree`: an `Iterable` trees its elements, anything else is itself.
is (1, (2, (3,))).tree.raku, '$((1, $((2, $((3,).Seq)).Seq)).Seq)', 'List.tree';
is (1..3).tree, (1, 2, 3), 'Range.tree';
is "x".tree, 'x', 'Str.tree is itself';
is Int.tree, Int, 'a type object is itself';
is ("x" ~~ /x/).tree.WHAT, Match, 'Match.tree is itself';

# A `Seq` or `Slip` with no settled shape is `Cool` too: its numeric methods
# read its element count.
is (1, 2, 3, 4).map(* + 0).sqrt, 2, 'Seq.sqrt';
is (1, 2, 3, 4).Slip.sqrt, 2, 'Slip.sqrt';
my $r = (1, 2, 3).map(* + 0).rand;
ok 0 <= $r < 3, 'Seq.rand is below its element count';
is (1, 2).map(* + 0).Rat, 2, 'Seq.Rat';

# `Any.chrs` (`self.list.chrs`) reaches a number, a `Range` and a `Seq`.
is 65.chrs, 'A', 'Int.chrs';
is (65..67).chrs, 'ABC', 'Range.chrs';
is (65, 66).map(* + 0).chrs, 'AB', 'Seq.chrs';

# `Blob.bytes` is `Blob:D:`, so a type object throws.
throws-like { Buf[uint8].bytes }, X::Parameter::InvalidConcreteness, 'Buf[uint8].bytes';
throws-like { Blob.bytes }, X::Parameter::InvalidConcreteness, 'Blob.bytes';
is Buf.new(1, 2).bytes, 2, 'Buf.bytes';

# `IO::Path.chars` is `Cool.chars` of the path; a `Buf` is not a `Str`.
is "abc".IO.chars, 3, 'IO::Path.chars';
throws-like { Buf.new(1).chars }, X::Buf::AsStr, 'Buf.chars';

# A numeric type object's `Rat` is `Int:D:` (Rakudo).
throws-like { Int.Rat }, X::Parameter::InvalidConcreteness, 'Int.Rat';
throws-like { Num.FatRat }, X::Parameter::InvalidConcreteness, 'Num.FatRat';
