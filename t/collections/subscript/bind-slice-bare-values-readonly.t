use Test;
plan 13;

my @bare;
@bare[0, 1] := (1, 2);
throws-like { @bare[0] = 3 }, X::AdHoc,
    'a slice-bound bare value has no writable container';
is @bare.raku, '[1, 2]', 'the rejected store preserves the slice values';

my @short;
@short[0, 1] := 1;
throws-like { @short[1] = 2 }, X::AdHoc,
    'a missing RHS value also binds the slice element read-only';
is @short.raku, '[1, Nil]', 'a short bound slice pads with Nil';

my @nested;
@nested[lazy 1, 2, (4, 5)] := 'a' ... *;
throws-like { @nested[1] = 'changed' }, X::AdHoc,
    'a lazy nested slice also binds bare values read-only';
is @nested[1], 'a', 'the lazy slice retains its first bound value';

my @nested-list;
@nested-list[1, (3, 4)] := 'a', 'b', 'c';
throws-like { @nested-list[3] = 'changed' }, X::AdHoc,
    'a nested list slice binds bare values read-only';
is @nested-list[3], 'b', 'the nested list slice retains its bound value';

my @shaped[2];
@shaped[0, 1] := 1, 2;
throws-like { @shaped[0] = 3 }, X::Assignment::RO,
    'a shaped slice refuses an assignment to a bare bound value';
is @shaped[0], 1, 'the rejected shaped store preserves the bound element';
throws-like { @shaped[1] = 3 }, X::Assignment::RO,
    'each shaped slice element raises the value assignment error';

my @source = 4, 5;
my @from-source;
@from-source[0, 1] := @source[0, 1];
lives-ok { @from-source[1] = 9 },
    'a slice whose source provides element containers stays writable';
is @from-source[1], 9, 'the writable slice accepts an element store';
