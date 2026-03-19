use Test;
plan 4;

is classify(-> $x { $x % 2 }, 1, 2, 3, 4)[0].elems, 2, "classify evens";
is classify(-> $x { $x % 2 }, 1, 2, 3, 4)[1].elems, 2, "classify odds";

is categorize(-> $x { $x % 2 == 0 ?? ("even") !! ("odd") }, 1, 2, 3, 4)["even"].elems, 2, "categorize evens";
is categorize(-> $x { $x % 2 == 0 ?? ("even") !! ("odd") }, 1, 2, 3, 4)["odd"].elems, 2, "categorize odds";
