use Test;

# `say` uses .gist, which for Set/Bag/Mix (and their mutable *Hash variants)
# shows the type-name wrapper: `Set(a b c)`, `Bag(a b(2))`, `Mix(a(1.5) b)`.
# Previously `say set(1,2,3)` dropped the wrapper and printed `1 2 3` because
# the fast gist path (gist_value) had no Set/Bag/Mix arm. The wrapper must
# match the `.gist` method exactly (shared helper), and nesting must work too.

plan 14;

# --- .gist produces the type-name wrapper (keys sorted for determinism) ---
is set(1, 2, 3).gist, 'Set(1 2 3)', 'Set.gist';
is bag(1, 2, 2).gist, 'Bag(1 2(2))', 'Bag.gist with multiplicity';
is (a => 1.5, b => 2).Mix.gist, 'Mix(a(1.5) b(2))', 'Mix.gist with weights';
is (1, 2, 3).SetHash.gist, 'SetHash(1 2 3)', 'SetHash.gist';
is (1, 2, 2).BagHash.gist, 'BagHash(1 2(2))', 'BagHash.gist';

# --- gist_value (the fast path used by say) must agree with .gist ---
is set(1, 2, 3).Str, '1 2 3' | '2 1 3' | '1 3 2' | '3 1 2' | '2 3 1' | '3 2 1',
    'Set.Str shows bare elements (no wrapper)';
isnt set(1, 2, 3).gist, set(1, 2, 3).Str, 'Set.gist differs from Set.Str';

# A Mix weight of exactly 1 is shown without the (1) suffix.
is (a => 1, b => 3).Mix.gist, 'Mix(a b(3))', 'Mix weight 1 omits the suffix';

# --- nested inside a Hash value: the element gists with its wrapper ---
is { x => set(1, 2) }.gist, '{x => Set(1 2)}', 'Set nested in a Hash value';

# --- .raku renders the proper constructor form (see t/setbagmix-raku-repr.t) ---
is set("a").raku, 'Set.new("a")', 'Set.raku uses Set.new(...)';

# --- empty collections ---
is set().gist, 'Set()', 'empty Set.gist';
is bag().gist, 'Bag()', 'empty Bag.gist';

# --- single-element determinism ---
is set("x").gist, 'Set(x)', 'single-element Set.gist';
is bag("x", "x", "x").gist, 'Bag(x(3))', 'single-key Bag.gist';
