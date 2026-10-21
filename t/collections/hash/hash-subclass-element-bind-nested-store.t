use Test;

# An `is Hash` / `is Array` subclass instance takes element binds and nested
# element stores into its backing storage, as a plain Hash/Array does:
#  - `$d<k> := $x` binds the element to `$x`'s container (it used to copy);
#  - `@a[0]<k> = v` / `@a[0]<k> := $x` / `%h<k><z> = v`, where the inner
#    container is such an instance, store into it -- the nested paths used to
#    replace the instance with a fresh Hash, or drop the write.
# Shapes from PDF's `PDF::COS::Dict` (`is Hash`), #12907. Expected values are
# the `raku` (v2026.09) output.

plan 10;

class D is Hash { }
class L is Array { }

{
    my $d = D.new;
    my $x = 1;
    $d<B> := $x;
    $x = 2;
    is $d<B>, 2, 'a bound key follows its source';
    $d<B> = 3;
    is $x, 3, '... and writes through to it';
}

{
    my %d := D.new;
    my $x = 1;
    %d<B> := $x;
    $x = 2;
    is %d<B>, 2, 'a %-bound instance binds a key too';
}

{
    my @a = D.new,;
    @a[0]<B> = 5;
    is @a[0]<B>, 5, 'a nested store into an instance element lands';
    isa-ok @a[0], D, '... and keeps the instance';
}

{
    my @a = D.new,;
    my $x = 1;
    @a[0]<B> := $x;
    $x = 2;
    is @a[0]<B>, 2, 'a nested bind into an instance element follows its source';
    isa-ok @a[0], D, '... and keeps the instance';
}

{
    my %h = k => D.new;
    %h<k><z> = 7;
    is %h<k><z>, 7, 'a store into an instance held in a Hash lands';
    isa-ok %h<k>, D, '... and keeps the instance';
}

{
    my @a = L.new,;
    @a[0][2] = 5;
    is @a[0][2], 5, 'an Array subclass element takes a nested positional store';
}
