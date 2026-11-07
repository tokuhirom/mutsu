use Test;

plan 8;

# `Seq.elems` of a `Seq.new($iterator)` nothing has pulled asks the iterator's
# `count-only`: an Array or Range iterator is not advanced, so it stays usable
# (Iter::Able's `n'th` calls `.elems` on such a Seq and then reads it).
my \it = [12, 3, 4, -5].iterator;
my $s = Seq.new(it);
is $s.elems, 4, 'elems of a Seq over an Array iterator';
is it.pull-one, 12, 'the iterator was not advanced';
is $s.elems, 3, 'elems follows the iterator cursor';
is-deeply $s.list, (3, 4, -5), 'the Seq is still readable';

my \r = (1..5).iterator;
my $t = Seq.new(r);
is $t.elems, 5, 'elems of a Seq over a Range iterator';
is r.pull-one, 1, 'the Range iterator was not advanced';

# A map iterator has no count-only: counting runs it.
my \m = (1, 2, 3).map({ $_ * 2 }).iterator;
my $u = Seq.new(m);
is $u.elems, 3, 'elems of a Seq over a map iterator';
ok m.pull-one =:= IterationEnd, 'the map iterator was drained by the count';

done-testing;
