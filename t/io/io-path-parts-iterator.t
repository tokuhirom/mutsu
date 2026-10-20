use Test;

# `IO::Path::Parts` is `Iterable`: its `iterator` gives the three part pairs,
# so `Any`'s methods written on `self.iterator` read them, while those written
# on `self.list` see the object as one item (#14034).

plan 20;

my $p = IO::Path::Parts.new('C:', '/d', 'f');

is $p.map(*.key).join(','), 'volume,dirname,basename', '.map iterates the pairs';
is $p.grep(*.value eq 'f').map(*.key).join(','), 'basename', '.grep reads the pairs';
is $p.first(*.value eq '/d').key, 'dirname', '.first reads the pairs';
is $p.head.key, 'volume', '.head is the first pair';
is $p.tail.key, 'basename', '.tail is the last pair';
is $p.head(2).map(*.key).join(','), 'volume,dirname', '.head(2)';
is $p.skip.map(*.key).join(','), 'dirname,basename', '.skip';
is $p.sort.map(*.key).join(','), 'basename,dirname,volume', '.sort sorts the pairs';
is $p.Seq.elems, 3, '.Seq holds the pairs';
is $p.batch(2).elems, 2, '.batch reads the pairs';
is $p.iterator.pull-one.key, 'volume', '.iterator pulls the first pair';

is $p.elems, 1, '.elems reads the object as one item';
is $p.list.elems, 1, '.list is the object as one item';
is $p.reverse.elems, 1, '.reverse reads the list';

is $p<>.^name, 'IO::Path::Parts', 'a zen slice is the object itself';
is $p[].^name, 'IO::Path::Parts', 'an empty positional subscript is the object itself';

my @keys;
for $p<> { @keys.push: .key }
is @keys.join(','), 'volume,dirname,basename', 'for over the decontainerized object iterates';

my @a = $p<>;
is @a.elems, 3, '@-assignment of the decontainerized object takes its pairs';
my @b = $p;
is @b.elems, 1, '@-assignment of the scalar keeps one item';

my @c;
for $p { @c.push: $_ }
is @c.elems, 1, 'for over the scalar runs once';
