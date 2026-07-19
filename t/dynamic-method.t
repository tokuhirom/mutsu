use Test;

# `.dynamic` on an Array/Hash reports whether the underlying *variable* is a
# dynamic one (declared with the `*` twigil: `@*a`, `%*h`). A plain `my @a` /
# `my %h` is not dynamic, and a literal `[...]`/`{...}` is never dynamic.
# `@`/`%` pass the container itself to the method, so `.dynamic` works directly
# (unlike `$x`, where you need `$x.VAR.dynamic`). List/Int/Str have no
# `.dynamic` at all.

plan 13;

# Array variables
{
    my @a;
    is @a.dynamic, False, 'my @a is not dynamic';
    my @*d;
    is @*d.dynamic, True, 'my @*d is dynamic';
}

# Hash variables
{
    my %h;
    is %h.dynamic, False, 'my %h is not dynamic';
    my %*d;
    is %*d.dynamic, True, 'my %*d is dynamic';
}

# Literals are never dynamic
is [1, 2, 3].dynamic, False, '[1,2,3].dynamic is False';
is { a => 1 }.dynamic, False, '{a=>1}.dynamic is False';

# A dynamic variable is visible (and still dynamic) from an inner scope
{
    my @*dyn = 1, 2, 3;
    sub reads-dyn { @*dyn.dynamic }
    is reads-dyn(), True, 'dynamic array seen from inner sub is still dynamic';
}

# A routine parameter array is not dynamic
{
    sub takes(@x) { @x.dynamic }
    is takes([1, 2, 3]), False, 'parameter array is not dynamic';
}

# `.dynamic` usable in boolean / expression context
{
    my @*x;
    is (@*x.dynamic ?? 'dyn' !! 'not'), 'dyn', '.dynamic in ternary';
    my @y;
    is (@y.dynamic ?? 'dyn' !! 'not'), 'not', 'non-dynamic in ternary';
}

# List / Int / Str do NOT have `.dynamic`
throws-like { (1, 2, 3).dynamic }, X::Method::NotFound, 'List has no .dynamic';
throws-like { (42).dynamic },      X::Method::NotFound, 'Int has no .dynamic';
throws-like { "foo".dynamic },     X::Method::NotFound, 'Str has no .dynamic';
