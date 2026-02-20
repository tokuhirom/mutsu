use Test;

plan 3;

# Anonymous class expression, create an instance
my $obj = class { method greet { 'hello' } }.new;
is $obj.greet, 'hello', 'can call method on anonymous class instance';

# Anonymous class with Str method
my $x = class { method Str { 'custom' } }.new;
is $x.Str, 'custom', 'anonymous class with custom Str method';

# Anonymous class as function argument
like class { method Str { 'foo' } }.new, /foo/, 'anonymous class instance stringifies via like';
