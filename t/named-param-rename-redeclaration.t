use Test;

# A renamed named parameter `:key($key)` binds the *inner* variable `$key`;
# the outer `key` is only the external argument name. mutsu used to count both
# the outer name (as `$key`) and the inner variable, falsely reporting
# X::Redeclaration of `$key`.

plan 7;

sub one(:key($key)) { $key }
is one(key => 5), 5, ':key($key) single renamed named param binds the inner var';

sub two(:key($key), :value($value)) { "$key=$value" }
is two(key => 'a', value => 'b'), 'a=b', 'two renamed named params do not clash';

# Inside pointy-block signatures across separate loops (the HTTP::Tiny pattern).
my %form = a => 1, b => 2;
my @out1;
for %form.sort -> ( :$key, :$value ) { @out1.push: "$key:$value" }
is @out1.sort.join(','), 'a:1,b:2', 'shorthand :$key destructuring works';

my @out2;
for %form.sort -> ( :key($key), :value($v) ) { @out2.push: "$key:$v" }
is @out2.sort.join(','), 'a:1,b:2', 'renamed destructuring in a separate loop works';

# Mixing shorthand and renamed forms across separate scopes is fine.
sub three(:$key) { $key }
is three(key => 9), 9, 'shorthand :$key in another sub does not clash with renamed form';

# A genuine clash must still be reported.
dies-ok { EVAL 'sub bad(:key($x), $x) { }' },
    'real duplicate ($x renamed + $x positional) still errors';
dies-ok { EVAL 'sub bad2(:key($x), :other($x)) { }' },
    'two renamed params binding the same $x still error';
