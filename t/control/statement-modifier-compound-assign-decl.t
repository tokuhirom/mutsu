use Test;

plan 2;

my $product *= $_ for 2, 3;
is $product, 6, 'a compound declaration is initialized once before a postfix for';

my $sum += $_ for 1, 2, 3;
is $sum, 6, 'a compound declaration accumulates across postfix for iterations';
