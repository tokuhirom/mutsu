use Test;

plan 5;

# A code-object call (`.&g`, `$x.&g`, `&g($x)`, `$code($x)`) binds a sigilless
# parameter to the argument variable's container, as `g($x)` does
# (P5chomp's `$_ = "b\n"; .&chomp` with `multi chomp(\s) { s .= chomp }`).
sub g(\s) { s .= uc; 1 }

$_ = 'b';
is .&g, 1, '.&g returns';
is $_, 'B', '.&g wrote through the topic';

my $x = 'c';
$x.&g;
is $x, 'C', '$x.&g wrote through $x';

my $y = 'd';
&g($y);
is $y, 'D', '&g($y) wrote through $y';

my &h = sub (\s) { s = 'set' };
my $z = 'e';
h($z);
is $z, 'set', 'a code variable call writes through its argument';
