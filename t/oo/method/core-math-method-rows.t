use Test;

# ADR-11276 §9.112: `Cool.Rat`, `Cool.FatRat` and `Any.tree` are rows, and the
# receivers the deleted `methods_0arg/dispatch_core_math.rs` arms answered
# reach them.

plan 15;

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
