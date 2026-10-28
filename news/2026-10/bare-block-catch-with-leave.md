# A bare block with both CATCH and LEAVE handles its exception

`sub k { { CATCH { default { ... } }; LEAVE ...; die "w" } }` ran the LEAVE and let
the exception escape: a bare block in tail position of a routine took the
phaser-scope lowering, which drops the `CATCH`. A block that carries a
`CATCH`/`CONTROL` is now lowered as an implicit `try` whose body is the phaser
scope, at every tail-block site (`Compiler::needs_phaser_block_scope`, #15252).
