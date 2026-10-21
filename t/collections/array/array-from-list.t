use Test;

# `Array.from-list(\list)` builds an Array over a reified list (#14134).

plan 6;

ok Array.^methods.map(*.name).Set<from-list>, "Array.^methods lists from-list";

my $a = Array.from-list((1, 2));
isa-ok $a, Array, "the result is an Array";
is $a.join(","), "1,2", "with the list's elements";

my @src = 1, 2;
my $b = Array.from-list(@src);
$b.push(3);
is @src.join(","), "1,2", "the source is not shared";
is $b.join(","), "1,2,3", "the result is mutable";
is Array.from-list(()).elems, 0, "an empty list";
