unit module WhereSub;

sub above-two(Int $b where $b > 2) is export { $b }
sub above-two-block(UInt:D $b where { $b > 2 }, UInt:D $size = 1) is export { $b * $size }
sub tally(Int $n where * %% 2) is export { $n div 2 }
