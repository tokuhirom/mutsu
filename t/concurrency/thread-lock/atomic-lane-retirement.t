use Test;

plan 1;

my $x = 0;
for ^20 -> $expected {
    $x = $expected;
    cas $x, -> $value { $value + 1 };
}

is $x, 20, 'plain assignments can repeatedly retire and recreate an atomic lane';
