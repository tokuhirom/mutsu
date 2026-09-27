use Test;

plan 20;

# A list literal that slips a genuinely lazy list (`(1, |[\*] 1..*)`) is itself
# lazy: the slipped list is its tail, reified only as far as it is read.
# mutsu used to force a 200,000-element prefix of the slipped scan, so the
# factorial table of Math::Handy (`(constant f = 1, |[\*] 1..*)[$n]`) ran out
# of memory, and a slipped map/gather tail was read as a single element.

is (1, |[\*] 1..*)[5], 120, 'slipped infinite scan is indexed lazily';
my $l = (1, |[\*] 1..*);
is $l[25], 15511210043330985984000000, 'a big-integer scan tail reifies on demand';
ok $l.is-lazy, 'the list with a lazy tail is lazy';

sub fact(Int(Cool) $n) { (constant f = 1, |[\*] 1..*)[$n] }
is fact(25), 15511210043330985984000000, 'constant with a lazy slipped tail';
is fact(0), 1, 'its plain head element';

is (1, |(1..*).map(* + 0))[3], 3, 'slipped infinite map pipe';

my $g = (1, |(lazy gather { take $_ for 1..* }));
is $g[3], 3, 'slipped lazy gather';
ok $g.is-lazy, 'a lazy gather tail keeps the list lazy';

ok (1, |(1...*)).is-lazy, 'a slipped infinite sequence keeps the list lazy';
is (1, 2, |(3..*).map(* * 2), 9).head(5), (1, 2, 6, 8, 10), 'head/tail parts around a lazy middle';

my @a = 0, |(1...*);
is @a[2], 2, 'array assigned from a lazy-tailed list';
ok @a.is-lazy, 'and it stays lazy';

is [0, |(1...*)][4], 4, 'bracket array with a slipped lazy tail';

is (<1 2 3> Z* 1, |map 1/(2 + *), 0..*), (1, 1, 1), 'zip against a lazy-tailed list';

ok (1, |(lazy 2, 3)).is-lazy, 'a slipped lazy-marked finite list keeps the list lazy';
is (1, |(1..3)).raku, '(1, 1, 2, 3)', 'a finite slip still flattens eagerly';

# A finite lazy-marked array (no user code to run) is reified by the mutators
# that need the whole array, as in Rakudo (roast S32-array/splice.t).
my @s = <a>, |lazy <b c d>;
is-deeply @s.splice(1, 10), [<b c d>], 'splice on a finite lazy-tailed array';
is-deeply @s, [<a>], 'and the array is spliced';
my @t = lazy <b c d>;
is @t.shift, 'b', 'shift on a lazy-marked cached array';
is-deeply @t, [<c d>], 'and the array is shifted';
