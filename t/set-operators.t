use Test;

plan 47;

# --- (elem) membership ---
ok "a" (elem) set(<a b c>), "(elem) true for present element";
nok "z" (elem) set(<a b c>), "(elem) false for missing element";
ok "a" (elem) bag(<a b a>), "(elem) works with Bag";
ok "a" (elem) mix(<a b a>), "(elem) works with Mix";

# --- (cont) containment ---
ok set(<a b c>) (cont) "a", "(cont) true for present element";
nok set(<a b c>) (cont) "z", "(cont) false for missing element";

# --- (|) union ---
my $u = set(<a b>) (|) set(<b c>);
isa-ok $u, Set, "(|) returns a Set";
is $u.elems, 3, "union has 3 elements";
ok "a" (elem) $u, "union contains 'a'";
ok "b" (elem) $u, "union contains 'b'";
ok "c" (elem) $u, "union contains 'c'";

# Bag union
my $bu = bag(<a a b>) (|) bag(<b b c>);
isa-ok $bu, Bag, "Bag (|) returns a Bag";
is $bu<a>, 2, "bag union: max count for 'a'";
is $bu<b>, 2, "bag union: max count for 'b'";
is $bu<c>, 1, "bag union: count for 'c'";

# --- (&) intersection ---
my $i = set(<a b c>) (&) set(<b c d>);
isa-ok $i, Set, "(&) returns a Set";
is $i.elems, 2, "intersection has 2 elements";
ok "b" (elem) $i, "intersection contains 'b'";
ok "c" (elem) $i, "intersection contains 'c'";

# Bag intersection
my $bi = bag(<a a b b c>) (&) bag(<a b b b>);
isa-ok $bi, Bag, "Bag (&) returns a Bag";
is $bi<a>, 1, "bag intersection: min count for 'a'";
is $bi<b>, 2, "bag intersection: min count for 'b'";
is $bi<c>, 0, "bag intersection: 'c' not in result";

# --- (-) difference ---
my $d = set(<a b c d>) (-) set(<b d>);
isa-ok $d, Set, "(-) returns a Set";
is $d.elems, 2, "difference has 2 elements";
ok "a" (elem) $d, "difference contains 'a'";
ok "c" (elem) $d, "difference contains 'c'";

# Bag difference
my $bd = bag(<a a a b b>) (-) bag(<a b b>);
isa-ok $bd, Bag, "Bag (-) returns a Bag";
is $bd<a>, 2, "bag difference: 3-1=2 for 'a'";
is $bd<b>, 0, "bag difference: 2-2=0 for 'b'";

# --- (^) symmetric difference ---
my $x = set(<a b c>) (^) set(<b c d>);
isa-ok $x, Set, "(^) returns a Set";
is $x.elems, 2, "symmetric difference has 2 elements";
ok "a" (elem) $x, "sym diff contains 'a'";
ok "d" (elem) $x, "sym diff contains 'd'";

# --- (<=) subset ---
ok set(<a b>) (<=) set(<a b c>), "proper subset is subset";
ok set(<a b c>) (<=) set(<a b c>), "equal set is subset";
nok set(<a b c d>) (<=) set(<a b c>), "superset is not subset";

# --- (>=) superset ---
ok set(<a b c>) (>=) set(<a b>), "proper superset is superset";
ok set(<a b c>) (>=) set(<a b c>), "equal set is superset";
nok set(<a b>) (>=) set(<a b c>), "subset is not superset";

# --- (<) proper subset ---
ok set(<a b>) (<) set(<a b c>), "proper subset";
nok set(<a b c>) (<) set(<a b c>), "equal set is not proper subset";

# --- (>) proper superset ---
ok set(<a b c>) (>) set(<a b>), "proper superset";
nok set(<a b c>) (>) set(<a b c>), "equal set is not proper superset";

# --- Empty set operations ---
is (set() (|) set(<a>)).elems, 1, "union with empty set";
is (set(<a>) (&) set()).elems, 0, "intersection with empty set";
is (set(<a>) (-) set()).elems, 1, "difference with empty set";

done-testing;
