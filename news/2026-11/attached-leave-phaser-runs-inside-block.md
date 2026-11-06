# A LEAVE phaser attached by `sub EXPORT` runs inside the importing block

FINALIZER 0.0.10 attaches `{ $*FINALIZER.FINALIZE }` to the block that `use`s it through
`$*R.find-attach-target('block').add-leave-phaser`, and exports `$*FINALIZER` from the same `EXPORT`.
The phaser ran after the block's scope had been left, so the block's imported `$*FINALIZER` was
gone and the closure read the stale binding of the `EXPORT` frame it was created in (`Nil`): nothing
was finalized. The block that is the body of the import region now runs the attached phasers itself,
right after its own LEAVE queue and before its scope exits (`exec_block_scope_op`; the region records
the `end` of the block it wraps). A `use` with no block of its own (a compunit top level) keeps the
old order. FINALIZER's `t/02-basic.rakutest` and `t/03-basic.rakutest` now match `raku`. Test:
`t/modules/import-export/export-leave-phaser-sees-exported-dynamic.t`.
