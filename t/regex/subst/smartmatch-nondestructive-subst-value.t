use Test;

# `$x ~~ S/.../.../` and `$x ~~ TR/.../.../` yield the substituted copy, not a
# smartmatch of it against `$x`, and leave `$x` alone (#13370: Qwiratry's
# `$selector ~~ S/^ '<' (.*) '>' $/$0/`).

plan 13;

my $s = '<abc>';
is ($s ~~ S/^ '<' (.*) '>' $/$0/), 'abc', 'S/// over a bracketed string strips the brackets';
is $s, '<abc>', 'the left side is not modified';
my $t = 'abc';
is ($t ~~ S/^ '<' (.*) '>' $/$0/), 'abc', 'S/// without a match yields the string itself';
is ($s ~~ S/a/X/), '<Xbc>', 'a plain S///';
is ($s ~~ S:g/b|c/Y/), '<aYY>', 'S:g///';
is ($s ~~ TR/a..c/A..C/), '<ABC>', 'TR///';
is $s, '<abc>', 'TR/// leaves the left side alone';

sub strip(Str $x --> Str) { $x ~~ S/^ '<' (.*) '>' $/$0/ }
is strip('<q>'), 'q', 'the value satisfies a Str return type';
is strip('q'), 'q', 'also without a match';

is ('hello' ~~ S/l/L/), 'heLlo', 'a literal on the left';
ok ('hello' !~~ S/l/L/), 'negated: the copy is compared with the left side like any value';

$_ = 'topic';
is (S/o/0/), 't0pic', 'S/// on the topic';
is $_, 'topic', 'the topic is not modified';
