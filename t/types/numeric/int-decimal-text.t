use Test;

# An Int's decimal text is written on the stack by one routine for `.Str`,
# `~`, interpolation and hash keys. These pin it at the edges.

plan 9;

my $min = -9223372036854775807 - 1;
my $max = 9223372036854775807;
is $min.Str, '-9223372036854775808', 'the most negative Int';
is ~$max, '9223372036854775807', 'the largest Int';
is "<{0}>", '<0>', 'zero interpolates';
is (-1).Str, '-1', 'minus one';
is 1000000.Str.chars, 7, 'trailing zeros are kept';

my %h;
%h{$_} = 1 for -10, 0, 10;
is-deeply %h.keys.sort.List, ('-10', '0', '10'), 'Int hash keys';
is (1, 22, 333).join(','), '1,22,333', 'join';
is "{$max + 1}", '9223372036854775808', 'a big Int still renders';
is (-42).gist, '-42', 'gist';
