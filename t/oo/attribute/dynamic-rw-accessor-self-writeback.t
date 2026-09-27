use Test;

plan 2;

class C {
    method set(:$value) {
        my %*DATA;
        self.value = $value;
        self.^name;
    }
}

my $name = 'value';
C.^add_method: $name, method () is rw {
    Proxy.new:
        FETCH => -> $self { %*DATA{$name} // Any },
        STORE => -> $self, $value { %*DATA{$name} := $value<> }
};

my $object = C.new;
is $object.set(:value<ok>), 'C',
    'a Proxy-backed dynamic rw accessor preserves the method invocant';
is $object.WHAT.^name, 'C',
    'the accessor store does not replace the object in its caller slot';
