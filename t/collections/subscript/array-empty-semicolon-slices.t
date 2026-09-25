use Test;

plan 7;

is [;].raku, '[Any]', 'one empty array slice yields Any';
is [;;].raku, '[Any, Any]', 'two empty array slices yield two Any values';
is [;1].raku, '[Any, 1]', 'a leading empty slice is preserved';
is [;;1].raku, '[Any, Any, 1]', 'multiple leading empty slices are preserved';
is [1;].raku, '[1]', 'a single trailing semicolon does not add a slice';
is [1;;].raku, '[1, Any]', 'an extra trailing semicolon adds an empty slice';
is [1,2;;3,4].raku, '[(1, 2), Any, (3, 4)]', 'empty slices stay between array sections';
