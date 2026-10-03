use Test;

# `@b is copy` is `my @b = arg`: a fresh, untyped Array whatever the argument
# was. A native or typed argument used to keep its type, so Crypt::RC4's
# `multi method RC4(@buf is copy --> Array)` failed its own return check
# ("expected Array but got Array") when handed a `my uint8 @`.

plan 9;

my uint8 @native = 1, 2, 3;
my Int @typed = 4, 5;

sub what(@b is copy) { @b.^name ~ ' ' ~ @b.of.^name }
is what(@native), 'Array Mu', 'a native argument copies into an Array';
is what(@typed), 'Array Mu', 'a typed argument copies into an untyped Array';
is what(<x y>), 'Array Mu', 'a List argument copies into an Array';

sub ret(@b is copy --> Array) { @b }
is-deeply ret(@native), [1, 2, 3], 'the copy passes an Array return check';

sub bump(@b is copy) { $_++ for @b; @b }
is-deeply bump(@native), [2, 3, 4], 'the copy is mutable';
is-deeply @native.List, (1, 2, 3), 'and the caller is untouched';

class C {
    method what(@b is copy) { @b.^name }
    multi method ret(@b is copy --> Array) { @b }
}
is C.what(@native), 'Array', 'a method parameter copies the same way';
is-deeply C.ret(@native), [1, 2, 3], 'a multi method too';

sub named(:@b is copy) { @b.^name }
is named(b => @native), 'Array', 'and a named parameter';
