use Test;

# `Type.new = v` on a Proxy subclass with its own `method new`: the user `new`
# builds the Proxy (with `:&FETCH, :&STORE` naming `my method`s of the class
# body) and the assignment runs its STORE.

plan 3;

my $stored;
class Box is Proxy {
    has $!value = 5;
    method new is raw { callwith :&FETCH, :&STORE }
    my method FETCH is raw { $!value }
    my method STORE(Mu $t is raw --> Nil) { $stored++; $!value = $t }
}

is (Box.new = <3>), <3>, 'assigning to a freshly built proxy';
is $stored, 1, 'its STORE ran';
my $b := Box.new;
$b = 8;
is $b, 8, 'the same proxy bound to a variable';
