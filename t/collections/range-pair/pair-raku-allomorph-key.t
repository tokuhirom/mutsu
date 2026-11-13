use Test;

plan 5;

# An allomorph key is a Str: Pair.raku uses the colonpair form when its
# string is identifier-like, and the full key `.raku` otherwise.
is (IntStr.new(2, "k1") => 7).raku, ':k1(7)', 'IntStr key, identifier-like';
is (IntStr.new(2, "2") => 7).raku, 'IntStr.new(2, "2") => 7', 'IntStr key, numeric string';
is (IntStr.new(2, "a b") => 1).raku, 'IntStr.new(2, "a b") => 1', 'IntStr key with a space';
is (NumStr.new(2e0, "x") => True).raku, ':x', 'NumStr key with a True value';
is (IntStr.new(2, "k1") => 7).key.^name, 'IntStr', 'the key keeps its type';
