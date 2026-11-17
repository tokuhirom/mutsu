use Test;

# A `.map` / `.grep` block's FIRST phaser belongs to the closure clone: it
# fires on the clone's first call, however many loops call it, and a block
# re-created on each pass of an enclosing loop fires once per pass (#17673).

plan 8;

my $f = 0;
my $b = { FIRST $f++; $_ };
(^2).map($b).elems for ^3;
is $f, 1, 'one block in a variable, three maps: FIRST fires once';

my $h = 0;
for ^3 { (^2).map({ FIRST $h++; $_ }).elems }
is $h, 3, 'a block literal re-created per pass fires once per pass';

my $k = 0;
my $d = { FIRST $k++; $_ };
(^2).grep($d).elems for ^2;
(^2).map($d).elems;
is $k, 1, 'grep and map share the clone\'s flag';

my $q = 0;
for ^2 {
    my $e = { FIRST $q++; $_ };
    (^2).map($e).elems;
    (^2).map($e).elems;
}
is $q, 2, 'each clone fires once across the maps that call it';

my $z = 0;
my $y = { FIRST $z++; $_ };
().map($y).elems;
is $z, 0, 'a map over nothing never calls the block, so FIRST waits';
(^1).map($y).elems;
is $z, 1, '... and fires on the first real call';

my $l = 0;
my $lb = { FIRST $l++; $_ };
(1..*).map($lb).head(2).List.elems for ^2;
is $l, 1, 'a lazy map shares the flag too';

my $seen = 0;
sub mk { -> $x { FIRST $seen++; $x } }
my $m1 = mk();
(^2).map($m1).elems for ^2;
(^2).map(mk()).elems;
is $seen, 2, 'closures cloned by separate calls each fire once';
