use Test;

plan 1;

sub calc ($_) { 99 when 13 }
my $j = any 1..5;
$j = any (gather $j».take).grep: { Nil !=== calc $_ };
lives-ok { $j == 3 },
    'forcing a junction built from a self-referential gather does not recurse';
