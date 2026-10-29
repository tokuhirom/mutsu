use v6;
use lib 't/fixtures/imported-multi-rw/lib';
use Test;
use RwSubst;

plan 5;

# A method call on a parameter inside an imported `is rw` multi runs on the
# parameter's container cell; it must read through to the value.
is size-of("hello"), 5, 'method call on a param of an imported rw multi';

# Method arguments that are `is copy` parameters are containers too.
my $s = 'The black cat climbed the green tree';
is fetch($s, 14, 7, "jumped from"), 'climbed', 'substr with copied Int params keeps its length';
is $s, 'The black cat jumped from the green tree', 'and the replacement went in';

my $t = 'foobar';
is fetch($t, 1, 4), 'ooba', 'rw proxy read through the same params';
fetch($t, 1, 2) = 'OO';
is $t, 'fOObar', 'rw proxy write';
