use Test;

plan 3;

multi sub trait_mod:<is>(Method:D $method, :$lazy-lock!) {
    my $package = $method.package;
    my $attribute = Attribute.new(:name('$!LOCK'), :type(Lock), :$package);
    $package.^add_attribute($attribute);
    $package.^add_method: 'LOCK', method LOCK() {
        $attribute.get_value(self) // $attribute.set_value(self, Lock.new)
    };
    my $wrapper = method (|c) {
        my &original = nextcallee;
        self.LOCK.protect: { original(self, |c) }
    };
    $method.wrap: $wrapper;
}

class Guarded {
    has @!values;

    method add() is lazy-lock { @!values.push(1) }

    method count() { @!values.elems }
}

for ^3 {
    my $guarded = Guarded.new;
    await do for ^4 {
        start { $guarded.add for ^1000 }
    }
    is $guarded.count, 4000,
        'concurrent Attribute.get_value/set_value initialization shares one lock';
}
