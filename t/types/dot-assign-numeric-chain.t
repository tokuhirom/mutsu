use Test;

plan 1;

my $value = '20';
$value .= Numeric.Rat;
is $value, 20, 'a .= method chain parses as one mutating call';
