use Test;

plan 9;

my $hammered := (1, 2, 3, 4, 5);

my @a = 1, [2, [3, [4, 5]]];
is-deeply @a.flat, @a.Seq, 'calling .flat on an array is a no-op';
is-deeply @a.flat(1), @a.Seq, 'calling .flat(1) on an array is a no-op';
is-deeply @a.flat(:hammer), $hammered, 'array.flat(:hammer)';
is-deeply @a.flat(1, :hammer), (1, 2, [3, [4, 5]]), 'array.flat(1, :hammer)';
is-deeply @a.flat(2, :hammer), (1, 2, 3, [4, 5]), 'array.flat(2, :hammer)';

my @b := 1, (2, (3, (4, 5)));
is-deeply @b.flat, $hammered, 'calling .flat on list flattens deeply';
is-deeply @b.flat(1, :hammer), (1, 2, (3, (4, 5))), 'list.flat(1, :hammer)';

ok (1..*).flat.is-lazy, 'flat propagates .is-lazy (method form)';
ok flat(1..*).is-lazy, 'flat propagates .is-lazy (sub form)';
