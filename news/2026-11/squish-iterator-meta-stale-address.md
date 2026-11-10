# A plain `squish` no longer inherits an earlier `squish(:as, :with)`'s callbacks

The metadata that makes `.squish(:as, :with).iterator` callback-aware was keyed
by the Seq's address and only dropped when `.iterator` ran. A `squish(:as)` Seq
that was never iterated left its entry behind, and a later plain `squish`
allocated at the same address picked it up, silently dropping elements
(ParaSeq `t/50-squish.t` test 18). Each entry now carries a weak handle on its
Seq, which pins the address while the entry exists, and dead entries are swept
as the table doubles.
