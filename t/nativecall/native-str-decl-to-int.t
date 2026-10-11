use Test;

# From IRC::Log::Colabti: `my int $h = my str $ = $line.substr(1,2)` is the
# "fast Str to int conversion" idiom; both sides are native, so rakudo emits
# nqp::coerce_si instead of raising.
plan 4;

my $line = "[05:36] x";
my int $hour   = my str $ = $line.substr(1,2);
my int $minute = my str $ = $line.substr(4,2);
is $hour, 5, 'declared native str coerces to int (leading zero)';
is $minute, 36, 'second field';
my int $abc = my str $ = "abc";
is $abc, 0, 'non-numeric native str gives 0';
my str $s = "12";
my int $v = $s;
is $v, 12, 'named native str still coerces';
