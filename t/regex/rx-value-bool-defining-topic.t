use Test;

# From 6.d on, a stored regex boolifies against the `$_` of the scope it was
# written in, not the caller's (rx-value-bool-map-topic.t pins the 6.c rule).
# Measured on rakudo 2026.09; the result must not depend on whether the
# regex has a source tree (`rx/a/`) or not.

plan 4;

my $letters := rx/<[a..z]>/;
my $plain := rx/a/;
my $assigned = rx/\w/;

is <a A !>.map({ if $letters { $_ } else { 'X' } }).join, 'XXX',
  'a bound character-class rx ignores the map topic';
is <a>.map({ so $plain }).join, 'False', 'so does a bound literal rx';
is <a>.map({ so $assigned }).join, 'False', 'and an assigned one';
$_ = 'a';
ok $letters.Bool, 'the defining scope topic is the one matched';
