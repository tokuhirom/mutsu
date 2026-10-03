use Test;

# `.elems` / `.end` on an array variable take a short dispatch lane (#9494);
# lazy, shaped, empty and growing arrays answer as before.

plan 9;

my @a = 1, 2, 3;
is @a.elems, 3, 'elems';
is @a.end, 2, 'end';

my @e;
is @e.elems, 0, 'elems of an empty array';
is @e.end, -1, 'end of an empty array';

my @l = 1..*;
throws-like { @l.elems }, X::Cannot::Lazy, 'a lazy array still refuses elems';

my @s[2;3];
is @s.elems, 2, 'a shaped array counts its first dimension';

my @ch = <a b c>;
my $n = 0;
loop (my Int $i = 0; $i < @ch.elems; $i = $i + 1) { $n++ }
is $n, 3, 'a C-style loop bound';

my @g;
my @seen;
for ^3 { @g.push: $_; @seen.push: @g.elems }
is-deeply @seen, [1, 2, 3], 'a growing array is recounted each time';

class C { has @.items; method count { @!items.elems } }
is C.new(items => <x y>).count, 2, 'an attribute array';
