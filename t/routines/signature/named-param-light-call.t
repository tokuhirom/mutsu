use Test;

plan 28;

# A routine with a typed and/or constant-defaulted named parameter, and/or a
# fast return type, takes the light call path. Every case below must behave as
# the general binder does; each is called more than once so a warm call-site
# cache is exercised too.

sub typed(Int $x, Bool :$flag = True --> Int) { $flag ?? $x !! -$x }
for 1..3 {
    is typed(5), 5, 'default used when the named argument is omitted';
    is typed(5, :flag(False)), -5, 'a supplied named argument wins over the default';
}
is typed(5, :flag), 5, 'a bare :flag is True';

my $five = 5;
sub only-type(Int $x, Str :$s) { $s.defined ?? "$x:$s" !! "$x:-" }
is only-type(1), '1:-', 'an untyped-default typed named parameter is the type object when omitted';
is only-type(1, :s<a>), '1:a', 'and takes the supplied value';
for 1..2 {
    throws-like { only-type(1, :s(5)) }, X::TypeCheck::Binding::Parameter,
        'a wrong named argument type is a binding failure';
}
my $str = "x";
my $yes = "yes";
throws-like { typed($str) }, X::TypeCheck::Binding::Parameter, 'a wrong positional type still fails';
throws-like { typed(1, :flag($yes)) }, X::TypeCheck::Binding::Parameter, 'a Str for a Bool named fails';

sub num-default(:$n = 42, :$m = 'm') { "$n $m" }
is num-default(), '42 m', 'untyped constant defaults';
is num-default(:n(1)), '1 m', 'one default overridden';
is num-default(:m<z>, :n(2)), '2 z', 'both overridden, any order';

# The return type is checked.
sub bad-ret(Int $x, :$k = 1 --> Int) { 'nope' }
throws-like { bad-ret(1) }, X::TypeCheck::Return, 'a wrong return value fails the return check';
sub nil-ret(Int $x, :$k = 1 --> Nil) { 5 }
ok nil-ret(1) === Nil, '--> Nil returns Nil';
sub ret-ok(Int $x, :$k = 1 --> Str) { "v$x$k" }
is ret-ok(2), 'v21', 'a matching return value passes';
is ret-ok(2, :k(3)), 'v23', 'with a supplied named argument too';

# return / fail inside the body
sub early(Int $x, Bool :$neg = False --> Int) { return -$x if $neg; $x }
is early(3), 3, 'fall-through return';
is early(3, :neg), -3, 'explicit return';
sub failing(Int $x, :$d = 1 --> Int) { fail "boom" if $x > 1; $x }
ok failing(5) ~~ Failure, 'fail bypasses the return check';
is failing(1), 1, 'and the success path still checks it';

# Recursion keeps each call's own bindings.
sub fact(Int $n, Int :$acc = 1 --> Int) { $n <= 1 ?? $acc !! fact($n - 1, :acc($acc * $n)) }
is fact(5), 120, 'recursive named-parameter routine';

# Unknown named arguments and surplus positionals still fail.
my $two = 2;
dies-ok { typed(1, $two) }, 'a surplus positional dies';
sub req(Int $x, Int :$must!) { $must }
my $one = 1;
dies-ok { req($one) }, 'a required named parameter missing dies';
is req(1, :must(7)), 7, 'and passes when supplied';
