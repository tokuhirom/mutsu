use Test;
plan 2;

# A sigilless alias as a named-argument value names the variable's container,
# so an `is raw` parameter writes through it (#16230).
sub w(:$p is raw) { $p = 5 }
sub via(|c) { w |c }

my $target;
my \alias = $target;
via :p(alias);
is $target, 5, 'named argument given as a sigilless alias writes through';

my $t2;
my \a2 = $t2;
w :p(a2);
is $t2, 5, 'direct call with a sigilless alias named argument';
