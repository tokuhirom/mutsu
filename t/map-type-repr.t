use Test;

# An immutable Map (Capture.hash, .Map coercion, Map.new) must report its type
# as `Map` and render as `Map.new((...))`, not as a mutable `Hash` / typed-hash.

plan 16;

# --- Capture.hash returns a Map ---
is \(a => 1).hash.^name, 'Map', 'Capture.hash is a Map';
is \(a => 1, b => 2).hash.^name, 'Map', 'multi-key Capture.hash is a Map';
is \(a => 1).hash<a>, 1, 'Capture.hash is still indexable';
is (1, 2, 3).Capture.hash.^name, 'Map', '.Capture.hash is a Map';

# --- .Map coercion ---
is %(a => 1).Map.^name, 'Map', '%h.Map is a Map';
is (a => 1, b => 2).Map.^name, 'Map', 'list.Map is a Map';

# --- Map.new ---
is Map.new('a', 1).^name, 'Map', 'Map.new is a Map';

# --- .WHAT agrees with .^name ---
is %(a => 1).Map.WHAT.^name, 'Map', '.WHAT.^name agrees';

# --- .raku renders as Map.new((...)) ---
is %(a => 1).Map.raku, 'Map.new((:a(1)))', 'Map.raku is Map.new form';
is %(a => 1, b => 2).Map.raku, 'Map.new((:a(1),:b(2)))', 'multi-key Map.raku';
is \(a => 1).hash.raku, 'Map.new((:a(1)))', 'Capture.hash.raku is Map.new form';

# --- non-identifier keys use arrow form ---
is Map.new('1', 'x').raku, 'Map.new(("1" => "x"))', 'numeric-key Map.raku uses arrow form';

# --- .raku round-trips through EVAL ---
is %(a => 1).Map.raku.EVAL.^name, 'Map', 'Map.raku round-trips to a Map';
is %(a => 1, b => 2).Map.raku.EVAL<b>, 2, 'round-tripped Map is indexable';

# --- gist is unchanged ---
is %(a => 1).Map.gist, 'Map.new((a => 1))', 'Map.gist form';

# --- a plain Hash still reports Hash (no over-tagging) ---
is %(a => 1).^name, 'Hash', 'a plain hash is still a Hash';
