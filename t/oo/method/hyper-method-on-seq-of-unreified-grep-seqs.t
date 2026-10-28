use Test;

plan 6;

# Each element of a `.map` Seq can itself be a `.grep` Seq whose callback has
# not run; `».method` has to reify it before calling the method
# (ML::TriesWithFrequencies' `words`: `.map({ $_».key.grep(...) })».join($sep)`).
is-deeply (<a b c>, <d e>).map({ $_.grep({ $_ ne 'b' }) })».join('-').List, ("a-c", "d-e"),
    '».join over a map Seq of grep Seqs';
is-deeply (<a b c>, <d e>).map({ $_.grep({ $_ ne 'b' }) })».join('').List, ("ac", "de"),
    'with an empty separator';
is-deeply (<a b c>, <d e>).map({ $_.grep(* ne 'b') })».elems.List, (2, 2),
    '».elems counts the reified elements';
is-deeply ((1 => 'a', 2 => 'b', 3 => 'c'), (4 => 'd',)).map({ $_».key.grep({ $_ != 2 }) })».join('+').List,
    ("1+3", "4"), 'the trie words shape';
is-deeply (<x y>, <z>).map({ $_.map({ $_.uc }) })».join.List, ("XY", "Z"), 'a map Seq of map Seqs';
is-deeply (<a b>, <c>).map({ $_.grep({ True }).Seq })».join('-').List, ("a-b", "c"), 'an explicit .Seq still works';
