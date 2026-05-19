my @data = 1..1000;
my $sum = 0;
for ^100 {
    $sum += @data.grep({ $_ %% 3 }).map({ $_ * 2 }).elems;
}
say $sum;
