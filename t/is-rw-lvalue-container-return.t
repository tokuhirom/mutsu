use Test;

plan 15;

# ADR-0058: an `is rw` routine returns a *container*, and the caller's
# assignment writes through it. Every shape below reaches a storage location
# through the routine's OWN parameters, which the older caller-side
# re-interpretation of the callee's tail expression could not resolve.

# 1. A hash element reached through a sigilless (`\c`) parameter.
{
    sub g(\c) is rw { return-rw c<a> }
    my %h;
    g(%h) = 1;
    is-deeply %h, {a => 1}, 'return-rw of a param-reached hash element assigns';
}

# 2. The same with a typed `%c` parameter.
{
    sub g(%c) is rw { return-rw %c<a> }
    my %h;
    g(%h) = 2;
    is-deeply %h, {a => 2}, 'return-rw through a %-sigil parameter assigns';
}

# 3. An array element reached through a parameter.
{
    sub g(\c) is rw { return-rw c[1] }
    my @a = 10, 20, 30;
    g(@a) = 99;
    is-deeply @a, [10, 99, 30], 'return-rw of a param-reached array element assigns';
}

# 4. The element must be created when it does not exist yet.
{
    sub g(\c, $k) is rw { return-rw c{$k} }
    my %h;
    g(%h, 'fresh') = 'v';
    is-deeply %h, {fresh => 'v'}, 'a missing key autovivifies on write';
}

# 5. A getter built on the same routine must NOT vivify.
{
    sub g(\c, $k) is rw { return-rw c{$k} }
    my %h;
    my $read = g(%h, 'absent');
    nok $read.defined, 'reading an absent key yields an undefined value';
    is-deeply %h, {}, 'reading an absent key does not vivify it';
}

# 6. Recursive descent: each level autovivifies, and the container survives
#    passing through the nested call's argument and its `return-rw`.
{
    sub g(\c, @s) is rw {
        @s.elems > 1 ?? return-rw g(c{@s[0]}, @s[1 .. *]) !! return-rw c{@s[0]}
    }
    my %h;
    g(%h, ['a', 'b']) = 1;
    is-deeply %h, {a => {b => 1}}, 'recursive descent autovivifies the whole path';

    my %deep;
    g(%deep, ['x', 'y', 'z']) = 'leaf';
    is-deeply %deep, {x => {y => {z => 'leaf'}}}, 'three-level recursive descent';
}

# 7. A container returned from a nested `is rw` call passes through unchanged.
{
    sub leaf(\c) is rw { return-rw c<b> }
    sub outer(\c) is rw { return-rw leaf(c<a>) }
    my %h;
    outer(%h) = 1;
    is-deeply %h, {a => {b => 1}}, 'a container returned by a nested is-rw call passes through';
}

# 8. A computed (ternary) tail — no single static expression to re-interpret.
{
    sub t(\c, $flag) is rw { $flag ?? return-rw c<x> !! return-rw c<y> }
    my %h;
    t(%h, True) = 7;
    t(%h, False) = 8;
    is-deeply %h, {x => 7, y => 8}, 'a ternary tail assigns through the taken branch';
}

# 9. The method form, including a type-object invocant and a slurpy.
{
    class I { method in(\c, *@s) is rw { return-rw c{@s[0]} } }
    my %h;
    I.in(%h, 'a') = 1;
    is-deeply %h, {a => 1}, 'a class-method lvalue return assigns';

    my %g = b => 0;
    I.in(%g, 'b') = 5;
    is-deeply %g, {b => 5}, 'a class-method lvalue return overwrites an existing element';
}

# 10. An instance-method lvalue return over the invocant's own attribute.
{
    class Box {
        has %.store;
        method at(\k) is rw { return-rw %!store{k} }
    }
    my $b = Box.new;
    $b.at('k') = 'v';
    is-deeply $b.store, {k => 'v'}, 'an instance-method lvalue return assigns';
}

# 11. The plain `is rw` accessor shape still works (the named-location path).
{
    my $value = 0;
    sub f() is rw { $value }
    f() = 9;
    is $value, 9, 'a bare-variable is-rw tail still assigns';
}

# 12. A bind chain through a variable holding a deferred entry.
{
    my %h;
    my $x := %h<a>;
    my $y := $x<b>;
    $y = 3;
    is-deeply %h, {a => {b => 3}}, 'a := chain through a deferred bind token autovivifies';
}
