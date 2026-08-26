use Test;

plan 32;

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

# my $.x read+write from INSIDE a method body, invoked on the type object
# (no instance at all) -- the headline case: $.counter++ must mutate the
# single class-level slot, not a per-call copy that dies with the frame.
{
    class Counter {
        my $.counter = 0;
        method imm() { return $.counter++ }
    }
    my @seen = (Counter.imm for ^5);
    is @seen, (0, 1, 2, 3, 4), 'my $.x++ inside a method persists across type-object calls';
    is Counter.counter, 5, 'the mutation is visible through the accessor afterwards';
}

# our $.x read+write from inside a method body.
{
    class OurCounter {
        our $.counter = 0;
        method imm() { return $.counter++ }
    }
    my @seen = (OurCounter.imm for ^3);
    is @seen, (0, 1, 2), 'our $.x++ inside a method persists across type-object calls';
    is OurCounter.counter, 3, 'our $.x mutation visible through the accessor afterwards';
}

# The type object and every instance share exactly ONE slot, and a mutation
# made from inside a method on one is visible from all the others.
{
    class Shared {
        my $.counter = 0;
        method imm() { return $.counter++ }
    }
    my $a = Shared.new;
    my $b = Shared.new;
    is $a.imm, 0, 'first instance call sees the initial value';
    is $b.imm, 1, 'second (different) instance call sees the first mutation';
    is Shared.imm, 2, 'a type-object call sees both instance mutations';
    is Shared.counter, 3, 'the accessor on the type object reflects every call';
    is $a.counter, 3, 'the accessor on instance a reflects every call';
    is $b.counter, 3, 'the accessor on instance b reflects every call';
}

# @.x / %.x class-level attributes are also cell-direct inside a method.
{
    class ArrCounter {
        my @.items;
        method add($x) { @.items.push($x); return @.items.elems }
    }
    is ArrCounter.add(1), 1, '@.x.push inside a method works (1)';
    is ArrCounter.add(2), 2, '@.x.push inside a method works (2)';
    is ArrCounter.items, (1, 2), '@.x accessor reflects both pushes';

    class HashCounter {
        my %.data;
        method set($k, $v) { %.data{$k} = $v; return %.data.elems }
    }
    is HashCounter.set('a', 1), 1, '%.x{k}= inside a method works (1)';
    is HashCounter.set('b', 2), 2, '%.x{k}= inside a method works (2)';
    is HashCounter.data<a>, 1, '%.x accessor reflects the first key';
    is HashCounter.data<b>, 2, '%.x accessor reflects the second key';
}

# A per-instance `has $.x` of the same name still shadows an inherited
# class-level `our $.x` from inside a method body too (matches the existing
# outside-a-method shadowing tests above).
{
    class ShadowBase {
        our $.val = 100;
    }
    class ShadowDerived is ShadowBase {
        has $.val = 5;
        method show() { return $.val }
    }
    is ShadowDerived.new.show, 5, 'instance attribute shadows class-level attribute inside a method';
}

# Introspection: `my $.x` / `our $.x` install a real, reflectable accessor
# method -- NOT an instance attribute.
{
    class Introspect {
        my $.counter = 0;
        method imm() { return $.counter++ }
    }
    ok 'counter' (elem) Introspect.^methods.map(*.name), 'my $.x shows up in .^methods';
    is Introspect.^can('counter').elems, 1, 'my $.x is visible to .^can';
    is Introspect.^attributes.elems, 0, 'my $.x is NOT an instance attribute (.^attributes stays empty)';
}
