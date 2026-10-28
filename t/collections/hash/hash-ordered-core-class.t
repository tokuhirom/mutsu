use Test;

# #15232 (Qwiratry `class Tuple is Hash::Ordered is export {}`): rakudo ships
# Hash::Ordered as a core insertion-ordered Hash subclass.

plan 17;

my %h is Hash::Ordered = zed => 1, alpha => 2;
%h<mid> = 3;
is %h.keys.join(","), "zed,alpha,mid", "a `is Hash::Ordered` hash keeps insertion order";
is %h.values.join(","), "1,2,3", ".values follows it";
is %h.pairs.map(*.key).join(","), "zed,alpha,mid", ".pairs follows it";
is %h.kv.join(","), "zed,1,alpha,2,mid,3", ".kv follows it";
is %h.map(*.key).join(","), "zed,alpha,mid", ".map walks in order";
is (for %h.pairs { .key }).join(","), "zed,alpha,mid", "iterating the pairs";

class Tuple is Hash::Ordered { }
my $t = Tuple.new;
$t<x> = 1; $t<y> = 2; $t<a> = 3;
is $t.keys.join(","), "x,y,a", "a subclass keeps insertion order";
is Tuple.^mro.map(*.^name).join(" "), "Tuple Hash::Ordered Hash Map Cool Any Mu", "its MRO";
ok $t ~~ Hash, "it is a Hash";
is $t.elems, 3, ".elems";
ok $t<y>:exists, ":exists";

my $gone = $t<x>:delete;
is $t.keys.join(","), "y,a", "deleting drops the key";
$t<x> = 9;
is $t.keys.join(","), "y,a,x", "re-adding puts the key at the end";

$t<y> = 5;
is $t.keys.join(","), "y,a,x", "re-assigning an existing key keeps its place";

my $o = Hash::Ordered.new(<k 1 j 2>.Slip);
is $o.^name, "Hash::Ordered", "Hash::Ordered.new";
is $o.keys.join(","), "k,j", "built from a flat key/value list";
$o = Hash::Ordered.new(("k" => 1), ("j" => 2));
is $o.raku, 'Hash::Ordered.new((:k(1),:j(2)))', ".raku";
