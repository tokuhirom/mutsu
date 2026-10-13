use Test;

# An empty array interpolated into a regex is an alternation with no
# branches: it never matches, rather than being a null regex (an exception)
# or vanishing from the pattern (mutsu#13152).

plan 11;

my @a;
nok 'x' ~~ /@a/, 'bare @a never matches';
nok 'x' ~~ /@a+/, 'quantified bare @a never matches';
is ~('x' ~~ /x @a*/), 'x', 'zero repetitions of it still match';
is ~('x' ~~ /x @a | x/), 'x', 'an alternation falls through to the other branch';
is ~('xy' ~~ /x [@a | y]/), 'xy', 'inside a group alternation too';
my regex r { @a+ }
nok 'x' ~~ /<r>/, 'inside a named regex';

nok 'x' ~~ /<@a>/, '<@a> never matches';
is ~('x' ~~ /x <@a>?/), 'x', '<@a>? matches zero times';
nok 'x' ~~ /<?@a> x/, '<?@a> fails';
is ~('x' ~~ /<!@a> x/), 'x', '<!@a> holds';

my @b = <a b>;
is ~('b' ~~ /@b/), 'b', 'a non-empty array still alternates';
