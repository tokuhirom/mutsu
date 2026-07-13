# Decompose timestamps into days/hours/minutes/seconds.
my $total = 0;
for ^100000 -> $i {
    my $ts      = $i * 97 + 13;
    my $days    = $ts div (60 * 60 * 24);
    my $hours   = ($ts mod (60 * 60 * 24)) div (60 * 60);
    my $minutes = ($ts mod (60 * 60)) div 60;
    my $seconds = $ts mod 60;
    $total += $days + $hours * 2 + $minutes * 3 + $seconds * 5;
}
say $total;
