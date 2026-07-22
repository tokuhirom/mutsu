use Test;

# `List`/`Slip`/`Array`/`Seq`.from-iterator($iterator) builds the named container
# by consuming a Raku Iterator. Used by custom `does Iterable` types
# (e.g. Hash::Agnostic's `method List { List.from-iterator(self.iterator) }`).

plan 12;

is List.from-iterator((1, 2, 3).iterator).raku, '(1, 2, 3)', 'List.from-iterator';
is Array.from-iterator((1, 2, 3).iterator).raku, '[1, 2, 3]', 'Array.from-iterator';
is Slip.from-iterator((1, 2, 3).iterator).^name, 'Slip', 'Slip.from-iterator is a Slip';
is Seq.from-iterator((1, 2, 3).iterator).^name,  'Seq',  'Seq.from-iterator is a Seq';

is List.from-iterator((1, 2, 3).iterator).elems, 3, 'List.from-iterator has all elems';
is-deeply Array.from-iterator((<a b c>).iterator), ['a', 'b', 'c'], 'Array.from-iterator elements';

# Empty iterator.
is List.from-iterator(().iterator).elems, 0, 'List.from-iterator of empty';

# Pairs survive (the Hash::Agnostic use case).
my @pairs = (:a(1), :b(2));
ok List.from-iterator(@pairs.iterator).are(Pair),  'List.from-iterator keeps Pairs';
ok Slip.from-iterator(@pairs.iterator).are(Pair),  'Slip.from-iterator keeps Pairs';
ok Array.from-iterator(@pairs.iterator).are(Pair), 'Array.from-iterator keeps Pairs';

# A partially-consumed iterator continues from its current position.
my $it = (10, 20, 30).iterator;
$it.pull-one;
is-deeply List.from-iterator($it), (20, 30), 'from-iterator resumes after pull-one';

# Slip flattens when interpolated into a list.
is (0, |Slip.from-iterator((1, 2).iterator), 3).elems, 4, 'Slip.from-iterator flattens in a list';
