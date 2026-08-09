use Test;

plan 3;

my $b1 = -> +a { a.join(',') };
is $b1(1, 2, 3), '1,2,3', 'sigilless one-arg slurpy as only param';

my $b2 = -> $x, +a { a.join(',') };
is $b2(1, 2, 3), '2,3', 'sigilless one-arg slurpy after a positional';

my $b3 = -> 'lit', +a { a.join(',') };
is $b3('lit', 2, 3), '2,3',
    'sigilless one-arg slurpy after a literal positional';

done-testing;
