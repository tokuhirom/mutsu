use Test;

plan 8;

# `my (...) = RHS` iterates the RHS with one level of decont (Rakudo
# List.STORE): a single scalar holding an Array flattens into its elements,
# while a comma list keeps itemized values whole. (DBIish walks result rows
# with `my ($col-name, $datatype-buf) = $row`.)

my $r = [1, 2];
my ($a, $b) = $r;
is $a, 1, 'scalar-held array flattens: first element';
is $b, 2, 'scalar-held array flattens: second element';

my ($c, $d) = $r,;
is-deeply $c, $[1, 2], 'trailing-comma list keeps the itemized array whole';
nok $d.defined, 'second target is Any for a one-item list';

my $item = $(1, 2);
my ($e, $f) = $item;
is $e, 1, 'itemized List flattens too';

my ($i, $j) = ([1, 2], [3, 4]);
is-deeply $i, $[1, 2], 'two-item RHS: each item kept whole';
is-deeply $j, $[3, 4], 'two-item RHS: second item';

# A single-element [Any] must survive scalar assignment/binding — an old
# cross-metaop normalization collapsed it to Nil (DBIish returns a NULL row
# as [Any] and binds it with `my \r = self._row`).
my \nullrow = [Any];
ok ?nullrow, 'bound [Any] array is a truthy one-element array';
