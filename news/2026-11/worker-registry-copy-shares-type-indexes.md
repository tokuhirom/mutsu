# A worker thread's registry copy shares the type-name indexes

ParaSeq runs every batch of a degree > 1 `map`/`grep` on a pool thread whose
interpreter shares its parent's registry until its first write. The batch
method `BlockMapper.run` declares an inner `sub set-phaser`, and re-recording
that sub on every call went through the registry write guard that drops the
class short-name and lexical type-name indexes. The copy-on-write clone the
write triggered deep-copied those indexes, the guard then threw them away, and
the next `nqp::getattr(self, Seq, ...)` rebuilt them from every class key.

The per-call record now goes through the routine-table write path that keeps
the indexes (and is skipped when unchanged), and the indexes sit behind an
`Arc`, so a registry copy shares them. Separately, the `GetBareWord` site memo
now also remembers a module's `my class` resolved to its lexical storage name,
so `ParaStats`, `BufferIterator` and friends no longer walk the whole bareword
chain on every read.

`@list.&hyperize(16, 3, :!auto).map({ $_ * 2 })` over 8000 elements (500
batches) went from 3.13G to 2.58G instructions. This is one slice of #17337;
the degree-3 block `map` is still well behind rakudo.
