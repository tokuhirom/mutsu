use Test;

# A Proxy subclass whose `new` hands its own `my method`s to `callwith
# :&FETCH, :&STORE`: the methods read and write the proxy's attribute.

plan 3;

class Box is Proxy {
    has $!value = 5;
    method new is raw { callwith :&FETCH, :&STORE }
    my method FETCH is raw { $!value }
    my method STORE(Mu $t is raw --> Nil) { $!value = $t }
}

my $b := Box.new;
$b = 8;
is $b, 8, 'STORE writes the attribute and FETCH reads it';
$b = 9;
$b = 10;
is $b, 10, 'repeated stores';
is (Box.new = <3>), <3>, 'assigning to a freshly built proxy';
