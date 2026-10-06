use Test;

# From the Injector distribution: `has %!bind{Str:D; Str:D; Str:D}`.
plan 9;

my %h{Str:D; Str:D; Str:D};
%h{"a"; "b"; "c"} = 5;
%h{"a"; "b"; "d"} = 6;
is %h{"a"; "b"; "c"}, 5, 'full-depth read';
is-deeply %h{"a"; "b"}.map(*.keys.sort).flat.List, ("c", "d"), 'both writes survive';
ok %h{"a"; "b"}:exists, 'partial-depth :exists';
nok %h{"a"; "z"}:exists, 'missing key does not exist';
is %h.keys.sort.List, ("a",), 'first-dimension keys';

my %i{Str};
%i{"x"; "y"} = 1;
%i{"x"; "z"} = 2;
is %i{"x"; "y"}, 1, 'single type dimension, first write';
is %i{"x"; "z"}, 2, 'second write into the same first key';
is %i<x>.keys.sort.List, ("y", "z"), 'nested hash holds both';
is %i{"q"; "r"}, (Any,), 'absent leaf reads as Any';
