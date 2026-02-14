use Test;
plan 4;

my @a;
is-deeply @a.splice(2, 3), [], "splice on empty array beyond end returns empty list";
is-deeply @a, [], "splice beyond end does not mutate empty array";

my @b = <a b c>;
is-deeply @b.splice(10, 1), [], "splice beyond end on populated array returns empty list";
is @b.join(" "), "a b c", "splice beyond end does not mutate populated array";
