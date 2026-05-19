my %h;
for ^10000 {
    %h{"key-$_"} = $_ * 2;
}
my $sum = 0;
for %h.values {
    $sum += $_;
}
say $sum;
