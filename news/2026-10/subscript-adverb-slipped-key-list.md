# `%h{|@keys}:kv` and the other subscript adverbs accept a slipped index

A slipped index (`%row{|@ids}:kv`, as Red's `ResultSeq` writes it) was lowered into the
adverb helper call as `|EXPR`, which the call then spread across its own argument list. The
adverb name shifted out of place and the slice failed. The slip is now dropped at lowering
time, since a slipped key list is the same flattened list a plain `%h{@keys}` slice takes.

Refs #14094.
