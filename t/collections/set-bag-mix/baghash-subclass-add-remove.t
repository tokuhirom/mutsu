use Test;

plan 6;

class MyBag is BagHash { }
my $m = MyBag.new;
$m.add("a");
$m.add("a");
$m.add("b");
is $m.elems, 2, 'add on a BagHash subclass inserts keys';
is $m<a>, 2, 'repeated add bumps the count';
$m.remove("a");
is $m<a>, 1, 'remove decrements the count';
$m.remove("b");
is $m.elems, 1, 'remove to zero drops the key';
is $m.^name, 'MyBag', 'still a MyBag';

my $p = BagHash.new;
$p.add("x");
is $p.elems, 1, 'plain BagHash add unchanged';
