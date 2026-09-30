use Test;

# Origin: Random::Names (ecosystem) draws names with `@!adjectives.grab`.
plan 9;

my @a = 1..6;
my $one = @a.grab;
ok $one ~~ Int, '.grab returns a single element';
is @a.elems, 5, 'and removes it';
my $two = @a.grab(2);
is $two.elems, 2, '.grab(2) returns two';
is @a.elems, 3, 'and removes them';
ok ($two.list (&) @a.list).elems == 0, 'grabbed elements are gone';
is @a.grab(*).elems, 3, '.grab(*) takes everything';
is @a.elems, 0, 'leaving the array empty';
nok @a.grab.defined, '.grab on an empty array is undefined';

class C { has @.x = 1..5; method g { @!x.grab } }
my $c = C.new; $c.g;
is $c.x.elems, 4, '.grab on an attribute array mutates it';
