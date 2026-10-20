use Test;

# Binding an element (`@a[1] := $x`, `%h<k> := $x`) used to be recorded in a
# name-keyed `__mutsu_bound_index::<name>` env side table, and a second bind
# of the same element of a shaped array died with "Cannot modify an immutable
# value" because of it. The element's own `ContainerRef` cell is the binding
# (ADR-0097), so the side table is gone and a re-bind is an ordinary bind.

plan 7;

{
    my @s[3];
    my $q = 1;
    @s[1] := $q;
    lives-ok { @s[1] := $q }, 'a shaped array element can be bound twice';
}

{
    my %h;
    my $w = 1;
    %h<k> := $w;
    %h<k> := $w;
    %h<k> = 3;
    is $w, 3, 'a re-bound hash element still writes through to its container';
}

{
    my @a = 1, 2, 3;
    my $z = 0;
    @a[1] := $z;
    @a[1] := $z;
    @a[1] = 4;
    is $z, 4, 'a re-bound array element still writes through to its container';
    @a[1]:delete;
    @a[1] = 5;
    is $z, 4, 'a deleted element no longer writes through';
    is @a[1], 5, 'the store after the delete lands in the array';
}

{
    my @a = 1, 2, 3;
    my $z = 0;
    @a[1] := $z;
    @a.splice(0, 1);
    @a[0] = 99;
    is $z, 99, 'a bound element keeps its binding when splice moves it';
    is-deeply @a, [99, 3], 'and the array holds the written value';
}
