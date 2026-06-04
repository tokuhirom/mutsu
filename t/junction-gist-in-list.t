use Test;

# A junction's .gist renders each eigenstate with .gist, so a nested Array
# eigenstate keeps its brackets: `all(4, [5 6])`, not `all(4, 5 6)`. This must
# hold even when the junction is itself an element of a List/Seq/Array being
# gisted (e.g. the result of a nodal hyper `>>.all`). Previously the list-level
# gist helper rendered a nested junction via .Str, dropping the brackets.

plan 10;

# direct junction gist (the baseline that already worked)
is [4, [5, 6]].all.gist, 'all(4, [5 6])', 'direct .all.gist keeps nested array brackets';
is [4, [5, 6]].any.gist, 'any(4, [5 6])', 'direct .any.gist keeps nested array brackets';

# junction nested inside a List / Array / Seq
my $j = [4, [5, 6]].all;
is ($j,).gist, '(all(4, [5 6]))', 'junction inside a List keeps brackets';
is [$j].gist, '[all(4, [5 6])]', 'junction inside an Array keeps brackets';
is ($j, $j).Seq.gist, '(all(4, [5 6]) all(4, [5 6]))', 'junction inside a Seq keeps brackets';

# nodal hyper `>>.all` / `>>.any` produce a List of junctions
is ([[2, 3], [4, [5, 6]]]>>.all).gist, '(all(2, 3) all(4, [5 6]))',
    '>>.all is nodal; nested-array eigenstate keeps brackets';
is ([[2, 3], [4, [5, 6]]]>>.any).gist, '(any(2, 3) any(4, [5 6]))',
    '>>.any is nodal; nested-array eigenstate keeps brackets';

# mixed list of junctions with and without nested arrays
is (any(1, [2, 3]), all(4, 5)).gist, '(any(1, [2 3]) all(4, 5))',
    'mixed junctions render each eigenstate with gist';

# nested list eigenstate renders with parens
is all(1, (2, 3)).gist, 'all(1, (2 3))', 'nested List eigenstate renders with parens';

# one/none kinds too
is [7, [8, 9]].none.gist, 'none(7, [8 9])', 'none junction keeps nested array brackets';
