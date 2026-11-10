use Test;

plan 11;

sub fresh { (1, 2, 3).map(* * 2) }

my $a = fresh();
$a does role { method foo { 2 } };
is-deeply $a.list.raku, '(2, 4, 6)', '.list of a lazy Seq with a role does';
my $b = fresh();
$b does role { method foo { 2 } };
is $b.elems, 3, '.elems of a lazy Seq with a role does';
my $c = fresh();
$c does role { method foo { 2 } };
is $c.iterator.pull-one, 2, '.iterator of a lazy Seq with a role does';
my $d = fresh();
$d does role { method foo { 2 } };
is $d.join(','), '2,4,6', '.join';
is $d.foo, 2, 'the role method still answers';

my $e = fresh() but role { method foo { 3 } };
is $e.list.raku, '(2, 4, 6)', '.list of a lazy Seq with a role but';
is $e.foo, 3, 'the role method of but';

my $g = gather { take 1; take 2 };
$g does role { method foo { 4 } };
is $g.list.raku, '(1, 2)', '.list of a gather Seq with a role does';
is $g.foo, 4, 'its role method';

my $h = fresh();
$h does role { };
is $h.sum, 12, '.sum';
my $i = fresh();
$i does role { };
is $i.Array.raku, '[2, 4, 6]', '.Array';
