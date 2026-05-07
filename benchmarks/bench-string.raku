# String operations - concat, split, join, regex matching
my $checksum = 0;

# String concatenation
my $s = "";
for 1..10000 -> $i {
    $s = $s ~ "x";
}
$checksum += $s.chars;

# Split and join
for 1..1000 -> $i {
    my @parts = "the quick brown fox jumps over the lazy dog".split(" ");
    my $joined = @parts.join("-");
    $checksum += $joined.chars;
}

# Regex matching
for 1..5000 -> $i {
    if "hello world 42 foo" ~~ /\d+/ {
        $checksum += 1;
    }
}

say "checksum = $checksum";
