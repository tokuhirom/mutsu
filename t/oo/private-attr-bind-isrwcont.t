use Test;
use nqp;

# `$!attr := VALUE` seats VALUE itself: the attribute stops being a writable
# container, and the next method call sees that (annotations' Binder, #15259).

plan 6;

class A {
    has $!v is default(Nil);
    method rw { nqp::isrwcont($!v) }
    method bind(Mu $x is raw) { $!v := $x }
    method assign(Mu $x is raw) { $!v = $x }
}

my $fresh = A.new;
ok $fresh.rw, 'a fresh attribute is a writable container';

my $assigned = A.new;
$assigned.assign(<1>);
ok $assigned.rw, 'assignment keeps it a container';

my $bound = A.new;
$bound.bind(<0>);
nok $bound.rw, 'binding a plain value ends the container in a later call';

# The same through a Proxy subclass whose STORE rebinds on its second call.
class Latch is Proxy {
    has $!value is default(Nil);
    has $!guard is default(False);
    method new is raw { callwith :&FETCH, :&STORE }
    my method FETCH is raw { $!value }
    my method STORE(Mu $topic is raw --> Nil) {
        nqp::if(
          nqp::isrwcont($!value),
          nqp::cas($!guard, False, True)
            ?? ($!value = $topic)
            !! ($!value := $topic))
    }
}
my $latch := Latch.new;
is ($latch = <0>), <0>, 'first store binds';
is ($latch = <1>), <0>, 'the bound attribute is no container: the second store is ignored';
is $latch, <0>, 'and reads back the first value';
