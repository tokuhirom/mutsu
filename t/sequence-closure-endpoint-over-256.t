use Test;

plan 6;

# A closure-generated sequence must continue until its value endpoint, even
# when reaching that endpoint requires more than the old 256-step limit.
my @values = 1, { $_ + 1 } ... 300;

is @values.elems, 300, 'closure sequence reaches a distant value endpoint';
is @values[*-1], 300, 'closure sequence includes its value endpoint';

nok (0, { die 'must stay deferred' } ... 999).is-lazy,
    'a closure sequence with a value endpoint reports non-lazy without reifying';
is (32, { ($_ / 2).narrow } ...^ Rat), (32, 16, 8, 4, 2, 1),
    'a strict comparison reifies through a matching type endpoint';
is-deeply infix:<^...>((1, { $_ + 2 }), 9).List, (3, 5, 7, 9),
    'left exclusion retains the closure generator';
is set(my @tens = 10, * + 10 ... 250), set(10, 20 ... 250),
    'set coercion reifies a finite closure-backed array';
