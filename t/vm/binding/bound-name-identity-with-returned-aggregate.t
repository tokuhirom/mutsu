use Test;

# Found working the `hyperize` distribution: `$hyper := @a.&hyperize(12, 1)`
# binds a name to the Array the routine returned, so the name is the very
# same container as `@a` (`=:=`), whereas an assigned copy is not.

plan 7;

my @a;
my sub pass-through(\it, $n) { $n == 1 ?? it !! 0 }

my $bound := 5;
$bound := @a.&pass-through(1);
ok $bound =:= @a, 'a rebound name is the Array its call returned';

my $fresh := @a.&pass-through(1);
ok $fresh =:= @a, 'a name declared bound to a call result is that Array';

my %h;
my $hbound := 5;
$hbound := %h.&pass-through(1);
ok $hbound =:= %h, 'same for a Hash';

my $assigned = @a;
nok $assigned =:= @a, 'an assigned name owns its own Scalar';

my @b;
my $other := 5;
$other := @b.&pass-through(1);
nok $other =:= @a, 'a different Array is not identical';

class C { }
my $obj := C.new;
my sub same(\o) { o }
my $bound-obj := 5;
$bound-obj := same($obj);
ok $bound-obj =:= $obj, 'a name bound to the object a call returned is that object';

my $copy = $obj;
nok $copy =:= $obj, 'an assigned copy of an object is a different container';
