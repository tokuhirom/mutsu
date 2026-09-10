use Test;

plan 8;

is Set.Str, "", "Set.Str is empty string";
is ~Set, "", "~Set is empty string";

is SetHash.Str, "", "SetHash.Str is empty string";
is ~SetHash, "", "~SetHash is empty string";

is Bag.Str, "", "Bag.Str is empty string";
is ~Bag, "", "~Bag is empty string";

is BagHash.Str, "", "BagHash.Str is empty string";
is ~BagHash, "", "~BagHash is empty string";
