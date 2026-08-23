use Test;

plan 2;

# A closure-generated sequence must continue until its value endpoint, even
# when reaching that endpoint requires more than the old 256-step limit.
my @values = 1, { $_ + 1 } ... 300;

is @values.elems, 300, 'closure sequence reaches a distant value endpoint';
is @values[*-1], 300, 'closure sequence includes its value endpoint';
