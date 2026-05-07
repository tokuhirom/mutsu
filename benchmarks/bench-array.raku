# Array operations - push, map, grep, sort
my $checksum = 0;

# Push
my @arr;
for 1..10000 -> $i {
    @arr.push($i);
}
$checksum += @arr.elems;

# Map
my @doubled = @arr.map(* * 2);
$checksum += @doubled[0] + @doubled[*-1];

# Grep
my @evens = @arr.grep(* %% 2);
$checksum += @evens.elems;

# Sort (descending)
my @sorted = @arr.sort({ $^b <=> $^a });
$checksum += @sorted[0];

# Reverse
my @rev = @arr.reverse;
$checksum += @rev[0];

say "checksum = $checksum";
