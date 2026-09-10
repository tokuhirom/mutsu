use v6;
use Test;

plan 8;

# `for @$h { $_ .= uc }` iterates the scalar's inner array and must alias its
# elements: a topic mutation writes back into the array (rakudo-verified).
# This is Text::CSV's header munge (`$_ .= uc for @$hdr`, 91_csv_cb.t 20-23).

my $hdr = ["foo", "bar"];
$_ .= uc for @$hdr;
is-deeply $hdr, ["FOO", "BAR"], 'topic .= mutation writes back through @$scalar';

my $h2 = ["a", "b"];
$_ = "X" ~ $_ for @$h2;
is-deeply $h2, ["Xa", "Xb"], 'topic = assignment writes back through @$scalar';

my @a = "p", "q";
$_ .= uc for @a.list;
is-deeply @a, ["P", "Q"], '@arr.list aliases the array elements too';

my @b = "r";
my $hb := @b;
$_ .= uc for @$hb;
is-deeply @b, ["R"], 'a :=-bound scalar derefs to the bound array';

my $block = ["m", "n"];
for @$block { $_ .= tc }
is-deeply $block, ["M", "N"], 'block form writes back too';

# Guard rails: shapes that must NOT gain a writeback.
my $s = "keep";
for $s { }
is $s, "keep", 'a plain scalar topic source is untouched by a read-only loop';

my $one = [1, 2];
for $one { }
is-deeply $one, [1, 2], 'single-item scalar wrap (for $x) is not element-written';

my $ro = [3];
for @$ro { }
is-deeply $ro, [3], 'read-only loop over @$x leaves the array alone';
