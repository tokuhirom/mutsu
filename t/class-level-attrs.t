use Test;

plan 11;

# our $.bar creates a class-level (shared) attribute with accessor
{
    class Foo {
        our $.bar = 23;
        our $.yada = 13;
    }

    is Foo.bar, 23, 'our $.bar accessor works on type object';
    is Foo.yada, 13, 'our $.yada accessor works on type object';
}

# Inherited class-level attributes
{
    class Parent {
        our $.shared = 42;
    }
    class Child is Parent {};

    is Child.shared, 42, 'inherited class attribute accessible on subclass';
}

# Assignment to class-level attributes
{
    class Writable {
        our $.val = 10;
    }

    Writable.val = 99;
    is Writable.val, 99, 'class attribute assignment works';
}

# my $.x creates a class-level attribute shared across instances
{
    class MyClass {
        my $.x;
    }
    my $a = MyClass.new;
    $a.x = 42;
    is $a.x, 42, 'my $.x assignment through instance works';
    my $b = MyClass.new;
    is $b.x, 42, 'my $.x is shared across instances';
}

# my $.x with initializer
{
    class MyInit {
        my $.x = 'hello';
    }
    my $obj = MyInit.new;
    is $obj.x, 'hello', 'my $.x with default value works';
}

# Instance attribute hides parent class-level attribute
{
    class Base {
        our $.val = 100;
    }
    class Derived is Base {
        has $.val = 5;
    }

    is Derived.new.val, 5, 'instance attribute takes priority over class-level';
    dies-ok { Derived.val }, 'class attr hidden by instance attr on type object';
}

# Inherited class-level attribute assignment
{
    class P2 {
        our $.shared = 1;
    }
    class C2 is P2 {};

    C2.shared = 77;
    is C2.shared, 77, 'inherited class attribute assignment works';
    is P2.shared, 77, 'assignment through subclass updates parent storage';
}
