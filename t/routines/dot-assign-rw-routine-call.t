use Test;

# `f() .= meth` assigns through the container an `is rw` routine returns, as
# `f() = f().meth` does, evaluating the call once (#19024).

plan 4;

my $calls = 0;
my $s = 'ab';
sub f() is rw { $calls++; $s }
f() .= uc;
is $s, 'AB', 'the rw routine\'s container is assigned';
is $calls, 1, 'the call is evaluated once';

my @a = <x y>;
sub elem() is rw { @a[1] }
elem() .= uc;
is-deeply @a, ['x', 'Y'], 'an element container returned by an rw routine';

sub plain { 'z' }
dies-ok { plain() .= uc }, 'a routine returning a value cannot be assigned through';
