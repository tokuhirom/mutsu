use Test;

# The positional-light binder receives Pair values by position. A named
# argument must therefore keep this call on the general binder, which rejects
# it instead of binding the Pair to the next positional parameter.

plan 6;

sub constant-default($x, $y = 2) { "$x/$y" }

is constant-default(1, 2), '1/2',
    'a full-arity call still uses the positional light path';
dies-ok { constant-default(1, :verbose) },
    'a bare named argument is not bound positionally';
dies-ok { constant-default(1, :verbose(True)) },
    'a valued named argument is not bound positionally';
is constant-default(3), '3/2',
    'the constant default still fills an omitted parameter';
sub positional-pair($x, $y = 2) { $y.^name }
is positional-pair(4, (y => 9)), 'Pair',
    'a parenthesized Pair remains a positional argument';
is constant-default(5, 6), '5/6',
    'the cache remains usable after a rejected named call';
