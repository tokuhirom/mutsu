use Test;

plan 14;

# A hyper operator hypers a Set/Bag/Mix operand as the Associative it is --
# over its keys, the dwim arrows picking the key set as for a Hash -- and the
# result takes the type of the operand that donates the structure. It used to
# walk the QuantHash as a list of Pairs and nest the results. (#9482)

my $r = set(1, 2) «∪» set(3);
isa-ok $r, Set, '«∪» over two Sets is a Set';
is $r.elems, 0, '... over the (empty) key intersection';
my @nested = (set(1),) «∪» (set(3),);
isa-ok @nested[0], Set, 'a Set nested in a list is hypered the same way';
is @nested[0].elems, 0, '... over its key intersection';

is-deeply (bag(<a a b>) »+« bag(<a c>)), bag(<a a a b c>), '»+« unions the keys';
is-deeply (bag(<a a b>) «+» bag(<a c>)), bag(<a a a>), '«+» intersects them';
is-deeply (bag(<a a b>) »+» bag(<a c>)), bag(<a a a b>), '»+» takes the left keys';
is-deeply (bag(<a a b>) «+« bag(<a c>)), bag(<a a a c>), '«+« takes the right keys';
is-deeply (bag(<a a b>) »-« bag(<a c>)), bag(<a b>), 'a missing Bag key reads 0; non-positive weights drop';
is-deeply (mix(<a b>) »*« mix(<a>)), mix(<a>), 'a missing Mix key reads 0';
is-deeply (set(<a b>) »~« set(<b c>)), set(<a b c>), 'Set membership values, truthy results';
is-deeply (1 «+« bag(<a>)), bag(<a a>), 'a scalar broadcasts; the Bag donates the type';

my %h = a => 1, b => 2;
is-deeply (%h »-« bag(<a c>)), %(a => 0, b => 2, c => -1), 'a Hash on the left donates a Hash';
is-deeply (bag(1, 1, 2) »+» 1).keys.sort.List, (1, 2), 'typed elements keep their type';
