use Test;

# `sort` (the sub form) called with no positional argument at all is a
# runtime error as of Rakudo 2022.07 ("Must specify something to sort"),
# unlike `sort(())`, where an empty list *was* specified and sorting it
# legitimately yields `()`. See raku-doc/doc/Type/List.rakudoc:874.
plan 6;

throws-like { sort }, X::AdHoc,
    message => /'Must specify something to sort'/,
    'bare `sort;` with no arguments dies';

throws-like { sort() }, X::AdHoc,
    message => /'Must specify something to sort'/,
    'sort() with explicit empty parens dies the same way';

throws-like { sort(:k) }, X::AdHoc,
    message => /'Must specify something to sort'/,
    'sort(:k) with only an adverb and no list dies too';

is-deeply sort(()), (), 'sort(()) — an explicit empty list — returns () without dying';

is-deeply (3, 1, 2).sort.List, (1, 2, 3), 'sort with a real list is unaffected';

is-deeply ((5, 1, 3) ==> sort()).List, (1, 3, 5),
    'sort() as a feed sink still receives the fed list as its argument';
