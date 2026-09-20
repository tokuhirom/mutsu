use Test;

plan 1;

my %values = '$p' => 'Falconer', '$y' => 'Hanzo';
my @keys = '$p', '$y';

is-deeply %values{|@keys}, ('Falconer', 'Hanzo'),
    'a hash slice accepts a positional slip of keys';
