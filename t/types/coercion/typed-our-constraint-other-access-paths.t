use v6;
use Test;

# A typed `our` variable's constraint lives on its container (ADR-0042), so
# every path that reaches the container must check it -- not only the declaring
# name (#10411). Measured against rakudo v2026.09.

plan 23;

package TP {
    our Int @a = 1, 2;
    our Int %h = a => 1;
    our Int $v = 1;
    our $u = 1;
}

# --- element stores through the package-qualified name -------------------
throws-like { @TP::a[5] = "x" }, X::TypeCheck::Assignment,
    'an element store through `@Pkg::a` is checked';
is @TP::a.elems, 2, '... and does not extend the array';
throws-like { @TP::a[0, 1] = 3, "x" }, X::TypeCheck::Assignment,
    'a slice store through `@Pkg::a` is checked';
throws-like { %TP::h<b> = "x" }, X::TypeCheck::Assignment,
    'an element store through `%Pkg::h` is checked';
ok %TP::h<b>:!exists, '... and does not add the key';
throws-like { %TP::h<b c> = 3, "x" }, X::TypeCheck::Assignment,
    'a slice store through `%Pkg::h` is checked';
@TP::a[2] = 3;
is @TP::a[2], 3, 'a valid element store through `@Pkg::a` is stored';
%TP::h<c> = 4;
is %TP::h<c>, 4, 'a valid element store through `%Pkg::h` is stored';

# A typed container passed to an untyped array parameter keeps its type.
sub store-first(@e, $x) { @e[0] = $x }
throws-like { store-first(Array[Int].new, "a") }, X::TypeCheck::Assignment,
    'an element store into a typed Array through an untyped parameter is checked';

# --- stash assignment ----------------------------------------------------
throws-like { TP::<$v> = "a" }, X::TypeCheck::Assignment,
    'a stash assignment `Pkg::<$v> = ...` is checked';
is $TP::v, 1, '... and leaves the value alone';
TP::<$v> = 5;
is $TP::v, 5, 'a valid stash assignment writes the variable';
TP::<$u> = 6;
is $TP::u, 6, 'a stash assignment to an untyped package scalar writes it';
TP::<$u> = (1, 2);
is $TP::u, (1, 2), 'a stash assignment stores a list as one item';
TP::<$u> := 3;
is $TP::u, 3, 'a stash bind `Pkg::<$u> := ...` rebinds the variable';

# --- symbolic assignment -------------------------------------------------
throws-like { ::('$TP::v') = "a" }, X::TypeCheck::Assignment,
    'a symbolic assignment `::("\$Pkg::v") = ...` is checked';

# --- a scalar bound to the qualified name --------------------------------
throws-like { my $r := $TP::v; $r = "a" }, X::TypeCheck::Assignment,
    'a write through `my $r := $Pkg::v` is checked';
is $TP::v, 5, '... and leaves the variable alone';
{
    my $r := $TP::v;
    $r = 8;
    is $TP::v, 8, 'a valid write through the alias reaches the package variable';
    $TP::v = 9;
    is $r, 9, 'and a write through the package name reaches the alias';
    throws-like { $TP::v = "a" }, X::TypeCheck::Assignment,
        'the package variable keeps its constraint after the bind';
}
{
    my $r := $TP::v;
    sub set-it($x) { $r = $x }
    set-it(10);
    is $TP::v, 10, 'a write through the alias from a routine reaches the package variable';
    throws-like { set-it("a") }, X::TypeCheck::Assignment,
        '... and is checked there too';
}
