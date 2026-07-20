use Test;

# An immutable Map gists as `Map.new((k => v, ...))` (sorted keys), not the
# `{...}` hash form — matching raku and the `.raku` renderer. A plain Hash still
# gists as `{...}`.

plan 8;

is Map.new((a => 1, b => 2)).gist, 'Map.new((a => 1, b => 2))', 'Map.gist uses Map.new(...)';
is %(x => 10, y => 20).Map.gist, 'Map.new((x => 10, y => 20))', '.Map coercion gists as Map.new';

# `Foo.enums` returns a Map
enum Mass ( mg => 1/1000, g => 1/1, kg => 1000/1 );
is Mass.enums.gist, 'Map.new((g => 1, kg => 1000, mg => 0.001))', 'enum .enums gists as Map.new';

# put / Str is unchanged (tab-separated pairs)
is Map.new((a => 1)).Str, "a\t1", 'Map.Str is tab-separated, not Map.new';

# A plain Hash still gists as {...}
is { a => 1, b => 2 }.gist, '{a => 1, b => 2}', 'plain Hash still gists as {...}';

# Nested Maps in a list render recursively (both say and .gist paths)
is [Map.new((a => 1)), Map.new((b => 2))].gist,
   '[Map.new((a => 1)) Map.new((b => 2))]', 'nested Maps in a list (.gist method)';

# Keys are sorted
is Map.new((z => 1, a => 2, m => 3)).gist, 'Map.new((a => 2, m => 3, z => 1))', 'Map keys are sorted';

# A Map nested inside a Hash value
is { outer => Map.new((inner => 1)) }.gist, '{outer => Map.new((inner => 1))}',
   'Map nested in a Hash value';
