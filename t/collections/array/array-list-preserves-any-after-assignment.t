use v6;
use Test;

plan 6;

{
    my @a = (Any, 1, 2, 3);
    @a[1] = 99;

    is @a[0], Any, 'an untouched Any element remains directly readable';
    is @a.List.raku, '(Any, 99, 2, 3)', '.List preserves an untouched Any after a single-index assignment';
}

{
    my @a = (Any, 1, 2, 3);
    @a[1, 2] = @a[2, 1];

    is @a[0], Any, 'an untouched Any element remains after a slice assignment';
    is @a.List.raku, '(Any, 2, 1, 3)', '.List preserves an untouched Any after a slice assignment';
}

{
    my @a;
    @a[0, 2] = (Any, 'two');
    @a[2]:delete;

    is @a.elems, 1, 'deleting a sparse slice tail keeps the explicitly assigned Any';
    is @a[0], Any, 'the explicitly assigned Any remains after deleting a sparse slice tail';
}
