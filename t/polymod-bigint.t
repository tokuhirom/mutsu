use Test;

# `polymod`'s exact path was capped at i128, so an invocant wider than that fell
# through to an f64 loop that cannot represent the number: a finite divisor list
# produced zeros and an infinite one (`256 xx *`) produced nothing at all.
# Integer operands now decompose in arbitrary precision.
# Found via `Digest::MD5`'s test, which compares against
# `Blob.new(parse-base($hex, 16).polymod(256 xx *).reverse)`.

plan 11;

my $md5 = parse-base('900150983cd24fb0d6963f7d28e17f72', 16);

is $md5.polymod(256 xx *).join(','),
    (114, 127, 225, 40, 125, 63, 150, 214, 176, 79, 210, 60, 152, 80, 1, 144).join(','),
    'a 128-bit Int decomposes into its base-256 digits';
is $md5.polymod(256 xx *).elems, 16, '... sixteen of them, with no trailing quotient';
is Blob.new($md5.polymod(256 xx *).reverse).list.fmt('%02x', ''),
    '900150983cd24fb0d6963f7d28e17f72',
    '... and rebuilds the original hex digest';

is $md5.polymod(256, 256, 256).join(','),
    (114, 127, 225, 11409262320051119695188490943784).join(','),
    'a finite divisor list keeps the remaining quotient, in full precision';

# The infinite form stops as soon as the invocant is exhausted.
is 1000.polymod(256 xx *).join(','), '232,3', 'a small Int needs only two digits';
is 255.polymod(256 xx *).join(','), '255', 'a single digit stops after one step';
is 0.polymod(256 xx *).elems, 0, 'zero decomposes into nothing';
is (2**200).polymod(2 xx *).elems, 201, 'a 201-bit Int yields 201 binary digits';

# Non-integer operands keep the rational/float behaviour they had.
is 5.Rat.polymod(0.3, 0.2).join(','), '0.2,0,80', 'a Rat invocant still decomposes exactly';
is 600.polymod(gather { take 5; take 6 }).join(','), '0,0,20',
    'a finite lazy divisor source is still forced';
is 1234.polymod(10, 10, 10).join(','), '4,3,2,1', 'the ordinary Int case is unchanged';
