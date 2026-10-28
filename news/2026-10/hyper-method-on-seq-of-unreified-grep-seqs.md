# `».method` on a Seq whose elements are unreified `.grep` Seqs

`(<a b c>, <d e>).map({ $_.grep({ $_ ne 'b' }) })».join('-')` answered `("", "")`: each element is a
`.grep` Seq that has not run, and the hyper call walked its (still empty) elements. The hyper loop now
reifies an element Seq first (ADR-0058), as it already did for the result of each call.
`ML::TriesWithFrequencies`'s `words` (`.map({ $_».key.grep(...) })».join($sep)`) was the real case:
`t/07-root-to-leaf-paths`, `t/08-overall-from-JavaTrie` and `t/09-method-pipelining` now pass, 12/12 files.
