use Test;

# A routine whose body compiles to TRIR (`sub sp($ch) { 1 }`) completed on its
# first, uncached call and returned before the name-keyed positional-light
# cache was filled, so every later call re-resolved the name through the
# general dispatch path (#17441). The cached dispatch must still observe
# everything the general path does: a later `.wrap`, a changed argument
# shape, a junction.

plan 6;

my sub sp($ch) { 1 }
my $c = ' ';
my $n = 0;
$n += sp($c) for ^50;
is $n, 50, 'repeated calls of a TRIR-compilable routine';

my sub add($a, $b) { $a + $b }
is (^5).map({ add($_, 1) }).join(','), '1,2,3,4,5', 'arguments change between cached calls';
is add(1.5, 2), 3.5, 'a different argument type after the cache is filled';

my sub twice($x) { $x * 2 }
twice(1) for ^5;
&twice.wrap(-> $x { callsame() + 100 });
is twice(3), 106, 'a wrap after the cache is filled reaches the wrapper';

my sub one($x) { $x + 1 }
one(1) for ^5;
my $j = one(1|2);
ok $j ~~ Junction, 'a junction argument autothreads after the cache is filled';
is ($j == 2|3).so, True, '... with each value';
