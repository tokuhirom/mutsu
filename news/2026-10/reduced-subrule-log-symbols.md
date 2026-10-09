# Reduced-subrule log keyed by interned symbols

The failure-path action replay log (`record_reduced_subrule`) allocated two
`String`s per subrule reduction: the entry name and the dedup-set key. It now
stores the atom's interned `lookup_sym` and uses an `FxHashSet`, converting to
`String` only for entries that are actually replayed. Together with the lazy
surviving-span set, `bench-yaml-parse` is back below its pre-#12383 level
(350.9M instructions / 397k allocations vs 357.1M / 416k) (#12409).
