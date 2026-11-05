use Test;

# Adjacent colonpairs written as one term (`:a:b`, `:a1:b2:c3`) are a List of
# all of them, as under rakudo's RakuAST frontend; the legacy frontend kept
# only the first pair.

plan 12;

my @a = :a:!b:42c;
is-deeply @a, [:a, :!b, :c(42)], 'list assignment flattens the run';

my @b = :a :b;
is-deeply @b, [:a, :b], 'a space between the pairs makes no difference';

my $c = :a:b;
is $c.raku, '$(:a, :b)', 'a scalar holds the List';
is $c.^name, 'List', '... of class List';
is $c.elems, 2, '... with every pair';

my $d = :a(1):b(2):c(3);
is $d.raku, '$(:a(1), :b(2), :c(3))', 'valued pairs';
my $f = :a1:b2:c3;
is $f.^name, 'List', 'numeric-suffix pairs (:a1 is a => 1)';

my $g = :a1;
is $g.^name, 'Pair', 'a single colonpair is still a Pair';
is (:a:b).raku, '(:a, :b)', 'parenthesized';
is [:a:b].raku, '[:a, :b]', 'in an array composer';

sub named(|c) { c.hash.elems }
is named(:a:b), 2, 'as arguments';

my @ran;
my $e = :a(@ran.push: 1) :b(@ran.push: 2);
is @ran.join(','), '1,2', 'every value is evaluated, in order';
