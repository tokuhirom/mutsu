use Test;

# Shaped hashes (#19283): an object hash declared with a key type per
# dimension, keyed by a list of one key of each, as rakudo 2026.11's
# `Hash::Shaped` makes it.

plan 25;

my %h{Str;Int};
%h{"a";1} = 5;
%h{"b";2} = 6;
is %h{"a";1}, 5, 'a multi-dimensional subscript reads the entry';
is %h{"b";2}, 6, 'another entry';
is %h.^name, 'Hash[Any,(Str,Int)]', '.^name';
is-deeply %h.shape, (Str, Int), '.shape is the dimension types';
is %h.keyof, List, '.keyof is List';
is %h.of, Any, '.of';
is-deeply %h.keys.sort.List, (("a", 1), ("b", 2)), 'the keys are the key lists';
is %h.elems, 2, '.elems';
ok %h{"a";1}:exists, ':exists of an entry';
nok %h{"a";2}:exists, ':exists of another key list';
is %h.raku, '(my Any %{Str;Int} = ("a", 1) => 5, ("b", 2) => 6)', '.raku';
is %h.gist, '{(a 1) => 5, (b 2) => 6}', '.gist';
is %h{"zz";9}, Any, 'a missing entry reads as the value type';

throws-like { %h<a> = 1 }, X::NotEnoughDimensions,
    message => 'Cannot assign to a 2 dimension hash with only 1 dimensions', 'too few keys';
throws-like { %h{"a";1;2} = 1 }, X::TooManyDimensions,
    message => 'Cannot assign to a 2 dimension hash with 3 dimensions', 'too many keys';
throws-like { %h{"a";"x"} = 1 }, X::TypeCheck::Binding,
    message => q[Type check failed in binding to 'key of dimension 2'; expected Int but got Str ("x")],
    'a key of the wrong type';

is %h{"a";1}:delete, 5, ':delete gives the value';
is %h.elems, 1, 'and removes the entry';

%h{"b";2} += 10;
is %h{"b";2}, 16, 'a compound assignment';
%h{"c";3} //= 7;
is %h{"c";3}, 7, '//= on a missing entry';

my Int %t{Str;Str} = ("x", "y") => 3;
is %t.gist, '{(x y) => 3}', 'initialized from a pair keyed by a list';
is %t.^name, 'Hash[Int,(Str,Str)]', 'a typed one';
throws-like { %t{"a";"b"} = "no" }, X::TypeCheck::Assignment, 'a value of another type';

my %u{Int;Int;Int};
%u{1;2;3} = 'x';
is %u{1;2;3}, 'x', 'three dimensions';
is-deeply %u.shape, (Int, Int, Int), 'their shape';
