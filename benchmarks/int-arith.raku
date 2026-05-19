my $sum = 0;
for ^100000 {
    $sum += $_ * 3 + 1;
}
say $sum;
