use Test;

plan 8;

role Rows does Sequence does Positional {
    has @.rows;
    method iterator { @!rows.iterator }
    method Seq { @!rows.Seq }
    method elems { @!rows.elems }
}
class Table does Rows { }

my $t = Table.new(:rows(1, 2, 3));
my $i = $t.item;

is-deeply $i.list.raku, '(1, 2, 3)', '.list on an itemized Sequence instance reads its iterator';
is $i.elems, 3, '.elems on an itemized instance counts the rows';
is-deeply $i.List.raku, '(1, 2, 3)', '.List on an itemized instance';
is-deeply $i.Array.raku, '[1, 2, 3]', '.Array on an itemized instance';
is-deeply $i.cache.raku, '(1, 2, 3)', '.cache on an itemized instance';
is-deeply (gather { take $_ for $i.list }).Array.raku, '[1, 2, 3]', 'a gather over the itemized .list';
is-deeply $t.list.raku, '(1, 2, 3)', 'the bare instance is unchanged';
is-deeply $i.map(*.self).Array.raku, '[1, 2, 3]', '.map on an itemized instance';
