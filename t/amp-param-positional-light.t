use Test;

plan 4;

# A plain `&` parameter is a scalar Callable binding.  It belongs on the same
# positional-light call path as a `$` parameter; this also exercises the local
# `&` lookup that must still shadow a package routine from inside the callee.
sub apply(&code, $value) { code($value) }
is apply(-> $n { $n * 2 }, 21), 42,
    'a positional & parameter binds and invokes its Callable';

sub callback() { 'package routine' }
sub invoke-shadow(&callback) { callback() }
is invoke-shadow({ 'lexical Callable' }), 'lexical Callable',
    'a positional & parameter shadows a same-named package routine';

sub ignored(&code, $value) { $value + 1 }
is ignored({ die 'must not be invoked' }, 41), 42,
    'an unused positional & parameter accepts a Callable';

my $sum = 0;
my &one = { 1 };
for ^100 { $sum = $sum + ignored(&one, 0) }
is $sum, 100, 'repeated positional & parameter calls preserve their binding';
