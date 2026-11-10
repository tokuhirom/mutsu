# `Block.has-phaser` and `Block.has-phasers`

ParaSeq's `duckmap` asks the mapper block which of `FIRST`/`NEXT`/`LAST` it carries
(`$mapper.has-phaser($_)`), and died with `No such method 'has-phaser' for invocant of type
'Block'`. Both methods now answer from the block's top-level phaser statements, next to
`Block.phasers` and `Block.callable_for_phaser` (#15375). rakudo additionally answers `LEAVE` for a
block whose `FIRST`/`NEXT`/`LAST`/`KEEP`/`PRE`/`POST` it wraps in one; that is an internal detail
of its phaser table and is not reproduced. ParaSeq's `t/48-duckmap.rakutest` now runs to the end;
its remaining difference is two `# TODO` assertions about `LEAVE` firing per iteration.
