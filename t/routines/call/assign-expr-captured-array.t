use Test;

# Found via Algorithm::Diff t/base.rakutest: `@a = @b = ()` used as an expression
# replaced the slot of an `@` variable captured by a named sub, detaching it.
plan 5;

my @s; sub ps { @s.push(2) }
(@s = ()); ps();
is @s.elems, 1, 'statement-position (@s = ()) keeps the capture';

my @b; sub pb { @b.push(2) }
my $x = (@b = ()); pb();
is @b.elems, 1, 'value-position assignment keeps the capture';

my (@p, @q);
sub pp { @p.push(1) }
sub pq { @q.push(2) }
pp(); pq();
@p = @q = ();
pp(); pq();
is ~@p, '1', 'chained assignment: first array';
is ~@q, '2', 'chained assignment: second array';

my %h; sub ph { %h<a> = 1 }
my $y = (%h = ()); ph();
is %h.elems, 1, 'hash capture survives expression assignment';
