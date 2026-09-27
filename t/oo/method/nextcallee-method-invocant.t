use Test;

plan 2;

class Parent {
    method value($number) { $number + 1 }
}

class Child is Parent {
    method value($number) {
        my &parent = nextcallee;
        parent(self, $number);
    }
}

is Child.new.value(41), 42,
    'nextcallee returns a method callable with its implicit invocant';

class Wrapped {
    method value() { 42 }
}

Wrapped.^lookup('value').wrap: method (|c) {
    my &original = nextcallee;
    original(self, |c);
};

is Wrapped.new.value, 42,
    'a method wrapper can forward its invocant through nextcallee';
