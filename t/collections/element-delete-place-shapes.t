use Test;

plan 23;

# `:delete` resolves its container through the variable's place (the declaring
# slot of a local, else the name): the same op over every container shape a
# local can hold (#14701). Expected values are rakudo's.

my $h = {a => 1, b => 2, c => 3};
is $h<a>:delete, 1, "scalar-held hash: deleted value";
is $h.keys.sort.join(","), "b,c", "scalar-held hash: key removed";

my $arr = [1, 2, 3, 4];
is $arr[3]:delete, 4, "scalar-held array: trailing delete";
is $arr.elems, 3, "scalar-held array: trailing hole trimmed";
is $arr[1]:delete, 2, "scalar-held array: inner delete";
is $arr[1]:exists, False, "scalar-held array: deleted slot does not exist";
is $arr.raku, '$[1, Any, 3]', "scalar-held array: hole in the middle";

sub f { my $x = %(p => 1, q => 2); my $r = $x<p>:delete; ($r, $x.keys.sort.join(",")) }
is-deeply f(), (1, "q"), "scalar-held hash local of a routine";

my %m = x => 1, y => 2;
my %g := %m;
is %g<x>:delete, 1, "bound alias: deleted value";
is %m.keys.join(","), "y", "bound alias: the aliased hash changed";

my @lz = 1, 2, 4 ... Inf;
is @lz[2]:delete, 4, "lazy array: deleted value";
is @lz[2]:exists, False, "lazy array: slot gone";
is @lz[3], 8, "lazy array: the tail is intact";

for ({k => 1, j => 2},) -> $e {
    is $e<k>:delete, 1, "for-bound hash: deleted value";
    is $e.keys.join(","), "j", "for-bound hash: key removed";
}

my $s = {a => 1} but role R { };
is $s<a>:delete, 1, "mixin-wrapped hash: deleted value";
is $s.keys.elems, 0, "mixin-wrapped hash: key removed";

my @n = 1, 2, 3;
is-deeply @n[1, 2]:delete, (2, 3), "slice delete";
is-deeply @n, [1], "slice delete trims the trailing holes";

my @nested = 1..6;
is-deeply @nested[(1, (2,))]:delete, (2, (3,)), "nested slice delete: shape";
is-deeply @nested, [1, Any, Any, 4, 5, 6], "nested slice delete: holes";

my %typed{Int};
%typed{1} = 'a';
%typed{2} = 'b';
is %typed{1}:delete, 'a', "object hash: deleted value";

class C {
    has %.attr = a => 1, b => 2;
    method d { %!attr<a>:delete; %!attr.keys.sort.join(",") }
}
is C.new.d, "b", "attribute hash";
