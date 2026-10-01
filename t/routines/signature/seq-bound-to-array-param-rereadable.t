use Test;

# A Seq bound to an `@` parameter is cached (Rakudo binds through `.cache`),
# so the callee can consume it more than once. Found via App::Moneymoor's
# t/15-invariants-property.rakutest (`@!transactions.reverse` passed to a
# `:@transactions` named parameter, then sorted and grepped).

plan 6;

my @x = 3, 1, 2;

sub two-sorts(@c) { my @a = @c.sort; my @b = @c.sort; "{@a} | {@b}" }
sub sort-grep(@c)  { my @a = @c.sort; my @b = @c.grep(* > 1); "{@a} | {@b}" }
sub named(:@c = ()) { my @a = @c.sort; my @b = @c.grep(* > 1).sort; "{@a} | {@b}" }
sub sorted-then-elems(@c) { my @a = @c.sort; @c.elems }

is two-sorts(@x.reverse), "1 2 3 | 1 2 3", 'two sorts of a reversed Seq';
is sort-grep(@x.map(* + 1)), "2 3 4 | 4 2 3", 'sort then grep of a mapped Seq';
is named(c => @x.reverse), "1 2 3 | 2 3", 'named @ parameter';
is sorted-then-elems(@x.reverse), 3, 'sort then elems';
is sort-grep((3, 1, 2).reverse), "1 2 3 | 2 3", 'literal list reverse';

my $s = @x.reverse;
sub count-it(@c) { @c.elems }
is count-it($s), 3, 'a Seq held in a scalar still binds';

done-testing;
