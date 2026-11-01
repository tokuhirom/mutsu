use Test;

# An `@`/`%` declared without a constraint has none to check on an element
# store, and a store into it skips the by-name type record (#14702). A slot a
# `:=` binds to a typed container, or a slot two declarations share, is not
# untyped. Checked against rakudo.
plan 9;

{
    my @a;
    @a[0] = 1;
    @a[1] = "x";
    is @a.join(','), '1,x', 'an untyped array takes any element';
    my %h;
    %h<a> = 1;
    %h<b> = "x";
    is %h<a>.Str ~ %h<b>, '1x', 'an untyped hash takes any value';
}

{
    my @b := Array[Int].new;
    throws-like { @b[0] = "x" }, X::TypeCheck::Assignment, 'a declaration-time := of a typed array keeps its constraint';
}

{
    my @c;
    @c := Array[Int].new;
    throws-like { @c[0] = "x" }, X::TypeCheck::Assignment, 'a later := of a typed array keeps its constraint';
}

{
    my @d;
    my &rebind = { @d := Array[Int].new };
    rebind();
    throws-like { @d[0] = "x" }, X::TypeCheck::Assignment, 'a := inside a closure keeps its constraint';
}

{
    my %e := Hash[Int].new;
    throws-like { %e<k> = "x" }, X::TypeCheck::Assignment, 'a := of a typed hash keeps its constraint';
}

{
    my Int @f;
    throws-like { @f[0] = "x" }, X::TypeCheck::Assignment, 'a typed array declaration still checks';
    my Int %g;
    throws-like { %g<k> = "x" }, X::TypeCheck::Assignment, 'a typed hash declaration still checks';
}

sub shared-slot() {
    { my Int @s; @s[0] = 1; }
    { my @s; @s[0] = "x"; return @s[0]; }
}
is shared-slot(), 'x', 'an untyped declaration after a typed one of the same name is untyped';
