# `gist` of lists, Seqs, hashes and pairs is a method-table row

`List`, `Seq`, `Hash` and `Pair` `.gist` are now registered rows that share one renderer
(`collection_gist`) with the dispatch cascade. The two duplicated per-element renderers and the
cascade's Array/Seq/Slip/Pair/Hash gist arms are deleted. Rendering is unchanged; a collection
holding an element with its own `gist`, or a cyclic one, still routes exactly as before.
Part of ADR-11276 (campaign issue #11276).
