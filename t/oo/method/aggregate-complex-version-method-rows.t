use Test;
plan 22;

# Cool.Complex is self.Numeric.Complex; Range and Seq reach Cool's Complex,
# UInt and Version rows (ADR-11276 §9.72, #12688). Checked against raku.
is (1, 2, 3).Complex, 3+0i, 'List.Complex is the element count';
is [1, 2].Complex, 2+0i, 'Array.Complex';
is {a => 1}.Complex, 1+0i, 'Hash.Complex';
is Map.new((a => 1)).Complex, 1+0i, 'Map.Complex';
is (1..5).Complex, 5+0i, 'Range.Complex';
is (1..^5.5).Complex, 5+0i, 'Range.Complex of a Rat endpoint';
is (1..*).Complex, Inf+0i, 'Range.Complex of an endless range';
is (1, 2, 3).Seq.Complex, 3+0i, 'Seq.Complex';
is ().Complex, 0+0i, 'an empty List is 0+0i';
isa-ok (1, 2).Complex, Complex, 'the answer is a Complex';

is "1+2i".Complex, 1+2i, 'Str.Complex still parses a Complex';
is "  3 ".Complex, 3+0i, 'Str.Complex trims';
isa-ok "foo".Complex, Failure, 'Str.Complex of a non-number is a Failure';
is 7.Complex, 7+0i, 'Int.Complex still answers';

is (1..5).Version, v1.2.3.4.5, 'Range.Version';
is (1, 2).Seq.Version, v1.2, 'Seq.Version';
is [1, 2].Version, v1.2, 'Array.Version';

is (1, 2, 3).Seq.UInt, 3, 'Seq.UInt';
is (1..^5.5).UInt, 5, 'Range.UInt';
is ().Seq.UInt, 0, 'Seq.UInt of an empty Seq';

# A Range with a fractional endpoint numifies to its element count.
is (1..^5.5).sqrt, 5.sqrt, 'Range.sqrt counts 1..^5.5 as five elements';
is (0.5^..^3).Int, 2, 'Range.Int of an excluded fractional start';
