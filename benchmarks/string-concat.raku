my $s = '';
for ^10000 {
    $s ~= 'x';
}
say $s.chars;
