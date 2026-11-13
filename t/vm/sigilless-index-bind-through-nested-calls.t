use Test;
my %h = a => 1, b => 2;
my @a = 10, 20, 30;
sub set-it(\x, $v) { x = $v }
sub relay(\y, $v) { set-it(y, $v) }
sub absent(Mu \c --> Bool) { c =:= Nil || c =:= Any }
my $cache = Map.new('a', 7, 'b', 2);
sub probe($k) { my \hit = $cache{$k}; absent(hit) ?? 'absent' !! hit }

my \e = %h<a>;
set-it(e, 5);
is %h<a>, 5, 'a sigilless term bound to a hash element writes through a sigilless parameter';
relay(e, 7);
is %h<a>, 7, 'and through a parameter relayed by another sigilless parameter';
my \f = @a[1];
relay(f, 99);
is @a[1], 99, 'an array element bound the same way';

is probe('a'), 7, 'a bound element read through a sigilless parameter';
is probe('zz'), 'absent', 'a missing element is absent';
nok $cache<zz>:exists, 'probing did not add the key';
is probe('b'), 2, 'repeated probes keep answering';

my $calls = 0;
sub counted() { $calls++ }
for ^3 { probe('a'); counted() }
is $calls, 3, 'a mainline counter written between probes keeps every write';
is $cache.keys.sort.join(','), 'a,b', 'the map keeps exactly its keys';

done-testing;
