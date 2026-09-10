use Test;

# .nodemap always returns a List (even from a real Array or a Seq), applying
# the block to each top-level node without descending. .deepmap preserves the
# container kind and itemizes a sublist only when its *parent* is a List, not
# when the parent is a real Array. These surface as nodal hypers in
# roast/S03-metaops/hyper.t (».nodemap / ».deepmap).

plan 12;

# --- nodemap returns a List ---
is [2, 3].nodemap(* + 1).WHAT.gist, '(List)', 'nodemap from Array returns a List';
is (1, 2).nodemap(* + 1).WHAT.gist, '(List)', 'nodemap from List returns a List';
is (1, 2, 3).Seq.nodemap(* + 1).WHAT.gist, '(List)', 'nodemap from Seq returns a List';
is [2, 3].nodemap(* + 1).gist, '(3 4)', 'nodemap result gist is a List';

# --- nodemap is shallow: a sublist node numifies ---
is [4, [5, 6]].nodemap(* + 1).gist, '(5 3)', 'nodemap does not descend (sublist numifies)';

# --- deepmap preserves the outer Array, descends to leaves ---
is [4, [5, 6]].deepmap(* + 1).gist, '[5 [6 7]]', 'deepmap keeps Array, nested Array not itemized';
is [4, [5, 6]].deepmap(* + 1).raku, '[5, [6, 7]]', 'deepmap raku has no spurious $ itemizer';

# --- deepmap itemization depends on the parent container ---
is (1, [2, 3]).deepmap(* + 1).raku, '(2, $[3, 4])', 'List parent itemizes a sublist';
is [1, [2, 3]].deepmap(* + 1).raku, '[2, [3, 4]]', 'Array parent does not itemize a sublist';
is (1, (2, 3)).deepmap(* + 1).raku, '(2, $(3, 4))', 'List parent itemizes a nested List';

# --- nodal hyper forms ---
is ([[2, 3], [4, [5, 6]]]>>.nodemap(* + 1)).gist, '((3 4) (5 3))',
    '».nodemap is nodal; each node mapped, result is a List';
is ([[2, 3], [4, [5, 6]]]>>.deepmap(* + 1)).gist, '([3 4] [5 [6 7]])',
    '».deepmap is nodal; preserves Array structure to the leaves';
