use Test;

# A Hash's .gist separates pairs with ", " (comma-space). This already held
# for a top-level hash, but when a Hash is nested inside a List/Array/Seq/Hash
# being gisted, the inner gist helper joined pairs with a bare space. A
# multi-pair nested hash therefore rendered `{a => 1 b => 2}` instead of
# `{a => 1, b => 2}`. This surfaces in nodal hypers like `>>.classify` /
# `>>.categorize` (roast/S03-metaops/hyper.t).

plan 8;

# top-level (already correct)
is {a => 1, b => 2, c => 3}.gist, '{a => 1, b => 2, c => 3}', 'top-level hash gist uses comma';

# hash nested in an Array / List / Seq
is [{a => 1, b => 2}, {c => 3, d => 4}].gist,
    '[{a => 1, b => 2} {c => 3, d => 4}]', 'hashes inside an Array keep commas';
is ({a => 1, b => 2},).gist, '({a => 1, b => 2})', 'hash inside a List keeps commas';
is ({a => 1, b => 2}, {c => 3}).Seq.gist,
    '({a => 1, b => 2} {c => 3})', 'hashes inside a Seq keep commas';

# hash nested inside a hash value
is {x => {a => 1, b => 2}}.gist, '{x => {a => 1, b => 2}}', 'nested hash value keeps commas';

# single-pair nested hash: no separator needed (regression guard)
is [{a => 1}, {b => 2}].gist, '[{a => 1} {b => 2}]', 'single-pair nested hashes unchanged';

# nodal hyper that produces a List of Hashes
is ([[2, 3], [4, [5, 6]]]>>.classify(* %% 2)).gist,
    '({False => [3], True => [2]} {True => [4 [5 6]]})',
    '>>.classify nodal result renders hash pairs with commas';
is ([[2, 3], [4, 5]]>>.categorize(* %% 2)).gist,
    '({False => [3], True => [2]} {False => [5], True => [4]})',
    '>>.categorize nodal result renders hash pairs with commas';
