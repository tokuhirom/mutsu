use Test;

plan 10;

# $::("name") - scalar symbolic dereference
{
    my $x = 42;
    is $::("x"), 42, '$::("x") resolves to $x';
}

# $::() with computed name
{
    my $foo = "hello";
    my $name = "foo";
    is $::($name), "hello", '$::($name) resolves variable by computed name';
}

# @::("name") - array symbolic dereference
{
    my @arr = 1, 2, 3;
    is @::("arr").elems, 3, '@::("arr") resolves to @arr';
    is @::("arr")[1], 2, '@::("arr")[1] indexes correctly';
}

# %::("name") - hash symbolic dereference
{
    my %h = a => 1, b => 2;
    is %::("h")<a>, 1, '%::("h")<a> resolves to %h<a>';
    is %::("h").elems, 2, '%::("h").elems returns correct count';
}

# Symbolic deref with string expression
{
    my $val = 99;
    my $prefix = "va";
    my $suffix = "l";
    is $::($prefix ~ $suffix), 99, 'symbolic deref with concatenated name';
}

# Non-existent variable returns Nil
{
    is $::("nonexistent"), Nil, 'symbolic deref of non-existent var returns Nil';
}

# &::("name") - code symbolic dereference
{
    my &func = { 42 };
    my $name = "func";
    is &::($name)(), 42, '&::($name)() calls symbolically dereferenced function';
}

# &::("name") with a named sub
{
    sub my-sub() { 99 }
    is &::("my-sub")(), 99, '&::("my-sub")() resolves named sub';
}
