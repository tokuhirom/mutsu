use Test;

# Method dispatch registers the receiver's attribute `is default(...)`
# values on every call, skipping entries that are already current (#9494).
# Two classes sharing an attribute name must each keep seeing their own
# default however their calls interleave.

plan 12;
class A {
    has $.x is rw is default(1);
    method d { $!x.VAR.default }
    method reset { $!x = Nil; $!x }
}
class B {
    has $.x is rw is default(2);
    method d { $!x.VAR.default }
    method reset { $!x = Nil; $!x }
}
my $a = A.new; my $b = B.new;
for ^3 {
    is $a.d, 1, 'A default';
    is $b.d, 2, 'B default';
    is $a.reset, 1, 'A Nil-assign restores its default';
    is $b.reset, 2, 'B Nil-assign restores its default';
}
