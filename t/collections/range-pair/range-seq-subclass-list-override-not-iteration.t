use Test;

plan 7;

# Iteration (`@`-assignment, `for`) of a Range/Seq subclass reads
# `self.iterator`; a `list` the subclass declares answers an explicit `.list`
# only (#16013).
class S is Range { method list { (7, 8).list } }
class Sq is Seq { method list { (7, 8).list } }
class T is Range { method iterator { (4, 5).iterator } }

my @d = S.new(0, 3);
is-deeply @d.List, (0, 1, 2, 3), '@-assignment ignores a Range subclass\'s own list';
my @f = Sq.new((1, 2, 3).iterator);
is-deeply @f.List, (1, 2, 3), 'and a Seq subclass\'s';

my @seen;
for S.new(0, 2) { @seen.push: $_ }
is-deeply @seen, [0, 1, 2], 'for ignores it too';

is-deeply S.new(0, 3).list, (7, 8), 'an explicit .list still uses it';

my @e = T.new(0, 3);
is-deeply @e.List, (4, 5), 'a subclass with its own iterator is iterated through it';
my @g;
for T.new(0, 2) { @g.push: $_ }
is-deeply @g, [4, 5], 'also in for';

class U is Range { method iterator { (1, 2).iterator }; method list { (7, 8).list } }
my @h = U.new(0, 3);
is-deeply @h.List, (1, 2), 'with both declared, iterator wins for assignment';
