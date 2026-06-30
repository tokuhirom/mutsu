use Test;

plan 14;

# An object hash (`%h{KeyType}`) declared in EXPRESSION position — e.g. as a
# call argument `gen my Int %j{Cool}` — must carry its declared value type so a
# missing-key read defaults to the element type, not Any.

sub gen(\h) {
    my Int $i = 0;
    h{$_} = ++$i for 'a'..'d';
}

gen my Int %j{Cool};

# Existing keys behave normally (plain Str-key adverbs keep working).
is %j<b>, 2, 'existing key value';
is (%j<b>:k), 'b', 'existing key :k';
is (%j<b>:!p), (b => 2), 'existing key :!p';
is %j.elems, 4, 'gen populated 4 keys';

# Missing-key reads default to the value type (Int), surviving expr-position decl.
is %j<Z>, Int, 'missing key value defaults to Int';
is (%j<Z>:!v), Int, 'missing key :!v defaults to Int';
is (%j<Z>:!p), (Z => Int), 'missing key :!p defaults to Int';
is (%j<b Z>:!p), (b => 2, Z => Int), 'mixed key :!p';

# Pair.Str stringifies a type-object value to "" in Str context (raku `eq`).
is (B => Any).Str, "B\t", 'Pair.Str of (B => Any) is "B\t"';
is (B => Int).Str, "B\t", 'Pair.Str of (B => Int) is "B\t"';
is (B => 5).Str, "B\t5", 'Pair.Str keeps a defined value';

# `is` compares two type-object pair values as equal (both stringify to "k\t").
is (B => Any), (B => Mu), 'is treats (B => Any) and (B => Mu) as equal';
is (b => 2, C => Any), (b => 2, C => Mu), 'is on a list of type-object pairs';

# A bare object hash (no value type) keeps a normal scalar default.
my %c{Cool};
%c<a> = 1;
is (%c<a>:!p), (a => 1), 'bare object hash existing key :!p';
