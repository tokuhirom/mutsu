use Test;
plan 6;

# Namespaced enum declaration
{
    enum Foo::Bar <a b c>;
    is Foo::Bar::a.Int, 0, 'namespaced enum variant a is 0';
    is Foo::Bar::b.Int, 1, 'namespaced enum variant b is 1';
    is Foo::Bar::c.Int, 2, 'namespaced enum variant c is 2';
}

# Namespaced enum with explicit values
{
    enum X::Y (red => 1, green => 2, blue => 3);
    is X::Y::red.Int, 1, 'namespaced enum with explicit value red';
    is X::Y::green.Int, 2, 'namespaced enum with explicit value green';
    is X::Y::blue.Int, 3, 'namespaced enum with explicit value blue';
}
