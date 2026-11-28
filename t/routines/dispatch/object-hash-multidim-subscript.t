use Test;

# From the Injector distribution: `has %!bind{Str:D; Str:D; Str:D}`.
plan 18;

my %h{Str:D; Str:D; Str:D};
%h{"a"; "b"; "c"} = 5;
%h{"a"; "b"; "d"} = 6;
is %h{"a"; "b"; "c"}, 5, 'full-depth read';
is %h{"a"; "b"}.sort.List, (5, 6), 'a partial subscript reads every entry under the prefix';
ok %h{"a"; "b"}:exists, 'partial-depth :exists';
nok %h{"a"; "z"}:exists, 'missing key does not exist';
is %h.keys.map(*.join("/")).sort.List, ("a/b/c", "a/b/d"), '.keys are the whole key lists';
is %h<a>.sort.List, (5, 6), 'a single key is a prefix too';
is %h{"a"; "b"}:exists.List, (True, True), 'partial :exists is one True per entry';
is %h{"q"; "b"}:exists.elems, 0, 'partial :exists of an absent prefix is empty';
is %h{"a"; "b"}:p.map(*.value).sort.List, (5, 6), 'partial :p';
is %h{"a"; "b"}:v.sort.List, (5, 6), 'partial :v';
is %h{"a"; "b"}:kv.elems, 4, 'partial :kv is flat';
is %h{"a"; "b"; "c"}:k.join("/"), "a/b/c", 'whole-key :k';
is %h{"a"; "b"}:delete.sort.List, (5, 6), 'partial :delete gives the removed values';
is %h.elems, 0, 'and removes them';

my %i{Str};
%i{"x"; "y"} = 1;
%i{"x"; "z"} = 2;
is %i{"x"; "y"}, 1, 'single type dimension, first write';
is %i{"x"; "z"}, 2, 'second write into the same first key';
is %i<x>.keys.sort.List, ("y", "z"), 'nested hash holds both';
is %i{"q"; "r"}, (Any,), 'absent leaf reads as Any';
