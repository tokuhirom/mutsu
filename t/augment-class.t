use Test;
use MONKEY-TYPING;

plan 9;

# Basic augment class
{
    class Foo {
        method bar() { 42 }
    }
    augment class Foo {
        method baz() { 99 }
    }
    is Foo.new.bar, 42, 'original method still works after augment';
    is Foo.new.baz, 99, 'augmented method works';
}

# Augment builtin Int
{
    augment class Int {
        method double() { self * 2 }
    }
    is 21.double, 42, 'can augment builtin Int';
}

# Augment builtin Str
{
    augment class Str {
        method shout() { self.uc ~ '!' }
    }
    is "hello".shout, 'HELLO!', 'can augment builtin Str';
}

# Augment class with attributes
{
    class Quux {
        method original() { 'orig' }
    }
    augment class Quux {
        has $.extra = 'default';
    }
    is Quux.new(extra => 'val').extra, 'val', 'augmented attribute works';
    is Quux.new.extra, 'default', 'augmented attribute default works';
}

# Augment visible on existing instances
{
    class Visible {
        method a() { 'a' }
    }
    my $obj = Visible.new;
    augment class Visible {
        method b() { 'b' }
    }
    is $obj.b, 'b', 'augmented method visible on existing instances';
}

# Multiple augments
{
    class Multi {
        method one() { 1 }
    }
    augment class Multi {
        method two() { 2 }
    }
    augment class Multi {
        method three() { 3 }
    }
    is Multi.new.three, 3, 'multiple augments work';
}

# Augment builtin Array
{
    augment class Array {
        method second() { self[1] }
    }
    is [10, 20, 30].second, 20, 'can augment builtin Array';
}
