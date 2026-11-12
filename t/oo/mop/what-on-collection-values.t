use Test;

# `.WHAT` is a macro: on a collection or number value it answers the value's
# own type, typed and shaped containers included, whatever variable holds it.

plan 20;

my %h = a => 1, b => [1, 2];
my $v = %h;
is $v.WHAT.^name, 'Hash', 'a Hash held in a scalar';
ok $v.WHAT === Hash, '$v.WHAT === Hash';
nok $v.WHAT === Int, '$v.WHAT === Int is false';
is %h.WHAT.^name, 'Hash', 'a Hash variable';

my Int %ti = a => 1;
is %ti.WHAT.^name, 'Hash[Int]', 'a typed Hash';
my $tv = %ti;
is $tv.WHAT.^name, 'Hash[Int]', 'a typed Hash held in a scalar';
my %oh{Int};
is %oh.WHAT.^name, 'Hash[Any,Int]', 'an object Hash';

my @a = 1, 2;
is @a.WHAT.^name, 'Array', 'an Array';
my Int @ia = 1;
is @ia.WHAT.^name, 'Array[Int]', 'a typed Array';
my int @na = 1, 2;
is @na.WHAT.^name, 'array[int]', 'a native array';
is (1, 2).WHAT.^name, 'List', 'a List';

is (a => 1).WHAT.^name, 'Pair', 'a Pair';
is (1/2).WHAT.^name, 'Rat', 'a Rat';
is (1/2).FatRat.WHAT.^name, 'FatRat', 'a FatRat';
is (1..3).WHAT.^name, 'Range', 'a Range';
is set(1).WHAT.^name, 'Set', 'a Set';
is bag(1).WHAT.^name, 'Bag', 'a Bag';
is 1i.WHAT.^name, 'Complex', 'a Complex';

class MyHash is Hash {}
is MyHash.new.WHAT.^name, 'MyHash', 'a Hash subclass instance';
is $v.WHAT.new.WHAT.^name, 'Hash', '.WHAT.new builds the same type';
