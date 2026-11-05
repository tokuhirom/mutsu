use Test;
plan 8;
class It does Iterable { method iterator() { (7, 8).iterator } }
class Plain { }
is (1, 2, 3).flat.join(","), "1,2,3", "a list of Ints flattens as itself";
is (1, (2, (3, 4)), 5).flat.join(","), "1,2,3,4,5", "nested lists flatten";
is (1, It.new, 9).flat.join(","), "1,7,8,9", "an Iterable instance is read through its iterator";
is (1, (2, It.new), 9).flat.join(","), "1,2,7,8,9", "also inside a nested list";
is (1, slip(2, It.new), 9).flat.join(","), "1,2,7,8,9", "also inside a Slip";
is (1, Plain.new, 3).flat.elems, 3, "a non-Iterable instance stays one item";
is (1, $[2, 3], 4).flat.elems, 3, "an itemized Array is not descended into";
is (1, "a", 2.5, Any, 3e0).flat.elems, 5, "mixed scalars";
