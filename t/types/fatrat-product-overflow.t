use Test;

plan 2;

my $r = FatRat.new(1, 3);
for ^40 {
    $r = $r * FatRat.new(7, 5);
}

is $r.Str,
    '233345.8988636899876685501289392835508332483925',
    'FatRat multiplication keeps arbitrary precision past i64';
is $r.^name, 'FatRat',
    'FatRat multiplication preserves the FatRat type';
