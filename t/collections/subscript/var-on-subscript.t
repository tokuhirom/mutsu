use v6;
use Test;

# `.VAR` on a subscript. ADR-0040 slice 3 established the model: which of the
# element's container / the element itself you get is decided by the SOURCE
# container. These are the two shapes that model did not reach — a slice, and a
# container carrying `is default(...)`. Every expectation is `raku` v2026.07's.

plan 16;

# --- a slice hands back a List of containers, and `.VAR` on a List is identity
{
    my @a = 1, 2;
    is @a[0, 1].VAR.^name, 'List', 'a comma slice answers List, not Scalar';
    is @a[0 .. 1].VAR.^name, 'List', 'a range slice too';
    is @a[^2].VAR.^name, 'List', '...including the `^n` spelling';
    is @a[0, 1].VAR.raku, '(1, 2)', 'and the slice VAR is the slice itself';
    my @i = 0, 1;
    is @a[@i].VAR.^name, 'List', 'an @-sigiled index is a slice too';
}
{
    my %h = a => 1, b => 2;
    is %h<a b>.VAR.^name, 'List', 'a multi-word <> slice answers List';
}

# --- the single-element path must NOT move -------------------------------
{
    my @a = 1, 2;
    my %h = a => 1;
    is @a[0].VAR.^name, 'Scalar', 'a single positional subscript still answers Scalar';
    is %h<a>.VAR.^name, 'Scalar', 'a single <> subscript too (it is not a slice)';
    is @a[0].VAR.name, '@a', '...and still names the container it lives in';
    my $i = 1;
    is @a[$i].VAR.^name, 'Scalar', 'a variable index is a single element';
    is @a[*-1].VAR.^name, 'Scalar', 'so is a Whatever index';
}

# --- `is default(...)` is a property of the container, so the element reports it
{
    my @nat is default(0) = 1, 2;
    is @nat[0].VAR.default, 0, 'an array element reports the container is-default';
    my %hd is default(5) = a => 1;
    is %hd<a>.VAR.default, 5, 'a hash element reports it too';
}
{
    # ...and without the trait, the declared element type, else Any.
    my @a = 1, 2;
    is @a[0].VAR.default.gist, '(Any)', 'an untyped array element defaults to Any';
    my Int @t = 1, 2;
    is @t[0].VAR.default.gist, '(Int)', 'a typed array element defaults to its type';
    my %p = a => 1;
    is %p<a>.VAR.default.gist, '(Any)', 'an untyped hash element defaults to Any';
}

done-testing;
